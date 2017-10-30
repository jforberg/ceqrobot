var webpack = require('webpack')
  , Copy = require('copy-webpack-plugin')

module.exports = {
  context: __dirname + '/src',
  entry: [
    './app.js'
  ],
  output: {
    filename: 'app.js',
    path: __dirname + '/dist'
  },
  plugins: [
    new Copy(
      [ { from: '../static' } ]
    ),
  ],
  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader',
        query: {
          presets: ['es2015', 'react']
        }
      },
    ]
  }
}
