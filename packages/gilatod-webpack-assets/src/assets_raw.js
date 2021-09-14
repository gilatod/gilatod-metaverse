const noArgs = []

const files = (ctx => {
    let keys = ctx.keys();
    let values = keys.map(ctx);
    return keys.reduce((o, k, i) => {
        o[k.substring(2)] = values[i];
        return o;
    }, {});
})(require.context('assets', true, /.*/));

export function getFiles() {
    return files;
}