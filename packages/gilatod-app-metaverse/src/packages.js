function createList(ctx) {
    let keys = ctx.keys();
    let values = keys.map(ctx);
    return keys.reduce((o, k, i) => {
        o[k] = values[i];
        return o;
    }, {});
}

const extensions = createList(
    require.context('./extensions', true, /\/package.lua$/));

export function getExtensions() { return extensions; }