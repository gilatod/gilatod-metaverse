'use strict';

const loader_utils = require('loader-utils');
const validateOptions = require('schema-utils');
const path = require('path');
const fs = require('fs');

const {
	luastring_eq,
	to_jsstring,
	to_luastring,
	lua: {
		LUA_ERRSYNTAX,
		lua_dump,
		lua_pop,
		lua_tojsstring,
		lua_tostring
	},
	lauxlib: {
		luaL_Buffer,
		luaL_addlstring,
		luaL_buffinit,
		luaL_loadbuffer,
		luaL_newstate,
		luaL_pushresult
	}
} = require('fengari');
const analyse_requires = require('./analyse_requires.js').analyse_requires;

const schema = {
	type: 'object',
	properties: {
		dependencies: {
			type: 'object',
			patternProperties: {
				'.*': { type: 'string' }
			}
		},
		strip: {
			type: 'boolean'
		}
	}
};

const fileExists = function(filename) {
	try {
		const s = fs.statSync(filename);
		return s.isFile();
	} catch(e) {
		return false;
	}
};

const resolver = function(dep, dir, resourcePath) {
	if (dep[0] == '.') {
		return dep;
	}
	if (dep[0] == '$') {
		return dep.substring(1);
	}

	const ext = path.extname(resourcePath);
	const splited = dep.split('.');
	const base = path.join(...splited);

	const base_path = base + ext;
	const base_path_js = base + '.js';
	const init_file = 'init' + ext;

	const resolve = function(dir) {
		// try path + ext
		const full_path = path.join(dir, base_path);
		if (fileExists(path.resolve(full_path))) {
			return full_path;
		}
	
		// try path + init.ext
		const full_path_2 = path.join(dir, base, init_file);
		if (fileExists(path.resolve(full_path_2))) {
			return full_path_2;
		}

		// try path + js
		const full_path_js = path.join(dir, base_path_js);
		if (fileExists(path.resolve(full_path_js))) {
			return full_path_js;
		}

		return null;
	};

	const res = resolve(dir) || resolve(path.dirname(dir));
	if (res) {
		return res;
	} else if (splited.length == 1) {
		return dep;
	} else {
		const relativePath = path.join(splited[0], 'src', ...splited.slice(1)) + ext;
		const index = dir.indexOf('/' + splited[0] + '/');
		if (index != -1) {
			return dir.substring(0, index + 1) + relativePath;
		} else {
			return relativePath;
		}
	}
};

exports.raw = true;
exports.default = function(source) {
	const callback = this.async();
	(async () => {
		const options = loader_utils.getOptions(this) || {};
		validateOptions(schema, options, 'Fengari Loader');

		if (typeof source === 'string') {
			source = to_luastring(source);
		} else if (!(source instanceof Uint8Array)) {
			let buf = new Uint8Array(source.length);
			source.copy(buf);
			source = buf;
		}

		let L = luaL_newstate();
		if (luaL_loadbuffer(L, source, null, null) === LUA_ERRSYNTAX) {
			let msg = lua_tojsstring(L, -1);
			throw new SyntaxError(msg);
		}

		let s = 'var fengari_web = require(\'fengari-web\');\n';
		let lua_dependencies = options.dependencies;
		let lua_dependencies_keys;
		if (lua_dependencies === void 0) {
			lua_dependencies = {};
			lua_dependencies_keys = analyse_requires(source);
			for (let i=0; i<lua_dependencies_keys.length; i++) {
				let lua_name = lua_dependencies_keys[i];
				/* skip the 'js' library (fengari-interop) as it's already included in fengari-web */
				if (lua_name === 'js') continue;
				/* if lua requires 'foo' then look for webpack dependency 'foo' */
				const srcPath = this.resourcePath.substring(
					0, this.resourcePath.indexOf('/src/') + 5);
				lua_dependencies[lua_name] =
					resolver(lua_name, srcPath, this.resourcePath);
			}
		} else {
			lua_dependencies_keys = Object.keys(lua_dependencies);
		}
		if (lua_dependencies_keys.length > 0) {
			s +=
				'var lua = fengari_web.lua;\n' +
				'var lauxlib = fengari_web.lauxlib;\n' +
				'var L = fengari_web.L;\n' +
				'var push = fengari_web.interop.push;\n' +
				'lauxlib.luaL_getsubtable(L, lua.LUA_REGISTRYINDEX, lauxlib.LUA_PRELOAD_TABLE);\n';
			for (let i=0; i<lua_dependencies_keys.length; i++) {
				let lua_name = lua_dependencies_keys[i];
				let require_path = lua_dependencies[lua_name];
				if (!require_path) { continue; }
				if (path.isAbsolute(require_path)) {
					this.addDependency(require_path);
				}
				s +=
					'lua.lua_pushcfunction(L, function(L){push(L, require(' + JSON.stringify(require_path) +')); return 1;});\n' +
					'lua.lua_setfield(L, -2, fengari_web.to_luastring(' + JSON.stringify(lua_name) + '));\n';
			}
			s += 'lua.lua_pop(L, 1);\n';
		}

		let chunkname = '\'@'+'.' + this.resourcePath + '\'';
		if (options.strip) {
			const writer = function(L, b, size, B) {
				luaL_addlstring(B, b, size);
				return 0;
			};
			let b = new luaL_Buffer();
			luaL_buffinit(L, b);
			if (lua_dump(L, writer, b, true) !== 0)
				throw new Error('unable to dump given function');
			luaL_pushresult(b);
			source = lua_tostring(L, -1);
			source = 'fengari_web.luastring_of(' + source.join(',') + ')';
			lua_pop(L, 1);
		} else {
			/* check if source is valid JS string */
			let stringsource = to_jsstring(source);
			if (luastring_eq(source, to_luastring(stringsource))) {
				source = JSON.stringify(stringsource);
			} else {
				source = 'fengari_web.luastring_of(' + source.join(',') + ')';
			}
		}
		return s + 'module.exports = fengari_web.load(' + source + ', ' + chunkname + ')' +
			/* call with require string */
			'.call(' + loader_utils.stringifyRequest(this, this.resource) + ');';
	})()
		.then((result)=>callback(null, result))
		.catch((err)=>callback(err));
};