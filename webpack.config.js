const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  mode: "development",
  entry: { index: "./src/index.js", jquery: "jquery/src/jquery" },
  devtool: "inline-source-map",
  devServer: {
    static: "./dist",
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: "Development",
      templateContent: `
          <html>
            <head>
              <script defer src="fontawesome/solid.min.js"></script>
              <script defer src="fontawesome/fontawesome.min.js"></script>
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
            </head>
            <body>
              <div id="aneemo_sortableList"></div>
            </body>
          </html>
        `,
    }),
  ],
  output: {
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "dist"),
    //clean: true,
  },
  optimization: {
    runtimeChunk: "single",
  },
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.less$/i,
        use: ["style-loader", "css-loader", "less-loader"],
      },
    ],
  },
};
