const path = require("path")
const HtmlWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");

module.exports = {
    entry: path.join(__dirname, "src", "bootstrap.lua"),
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "[name].[contenthash].js"
    },
    devServer: {
        contentBase: path.resolve(__dirname, "dist")
    },
    resolve: {
        alias: {
            "fengari-web": path.resolve(
                __dirname, "node_modules", "fengari-web"),
            "assets": path.resolve(__dirname, "assets")
        }
    },
    module: {
        rules: [
            {
                test: /\.m?js$/,
                loader: 'babel-loader',
                options: {
                    presets: [
                        [
                            '@babel/preset-env',
                            {
                                "targets": {
                                "ie": "9"
                                }
                            }
                        ]
                    ]
                }
            },
            {
                test: /\.lua$/,
                loader: "fengari-loader"
            },
            {
                test: /\.html$/,
                loader: "html-loader",
            },
            {
                test: /\.css$/,
                use: ["style-loader", "css-loader"]
            },
            {
                test: /\.(png|jpg|gif|txt|otf|ttc|svg|mp3|wav)$/,
                loader: "file-loader",
                options: {
                    name: "[path][name].[hash].[ext]",
                },
            }
        ]
    },
    plugins: [
        new CleanWebpackPlugin(),
        new HtmlWebpackPlugin({
            template: path.resolve(
                __dirname, "src", "template.html")
        })
    ],
    optimization: {
        moduleIds: "deterministic",
        runtimeChunk: "single",
        splitChunks: {
            cacheGroups: {
                vendor: {
                test: /[\\/]node_modules[\\/]/,
                name: "vendors",
                chunks: "all"
                }
            }
        }
    }
}