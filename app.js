'use strict';

require('./src/style.css');
require('./src/index.html');

var Elm = require('./src/conway.elm');
var node = document.getElementById('app');
Elm.Conway.embed(node);