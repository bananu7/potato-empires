'use strict';

var loadImages = function (prefix) {
  var images = {
    'grass': null,
    'water': null
  };

  for (var i in images) {
    images[i] = new Image();
    images[i].src = prefix + '/' + i + '.png';
  }

  return images;
}

var makeTiles = function (game) {
  var tiles = new Array(game.board.width);

  for (var i = 0; i < game.board.height; ++i) {
    tiles[i] = new Array(game.board.width);
  }

  for (var y = 0; y < game.board.height; ++y) {
    for (var x = 0; x < game.board.width; ++x) {
      var terrainType = Math.random() < 0.5 ? 'grass' : 'water';
      tiles[y][x] = terrainType;
    }
  }

  return tiles;
}

var renderTile = function (game, image, x, y) {
  game.ctx.drawImage(
    game.images[image],
    x * game.board.tileSize,
    y * game.board.tileSize,
    game.board.tileSize,
    game.board.tileSize
  );
}

var renderTerrain = function (game) {
  for (var y = 0; y < game.board.height; ++y) {
    for (var x = 0; x < game.board.width; ++x) {
      renderTile(game, game.board.tiles[y][x], x, y);
    }   
  }
}

var renderGrid = function (game) {
  game.ctx.beginPath();

  for (var x = 0; x < game.board.width; ++x) {
    game.ctx.moveTo(x * game.board.tileSize, 0);
    game.ctx.lineTo(x * game.board.tileSize, game.board.height * game.board.tileSize);
  }

  for (var y = 0; y < game.board.height; ++y) {
    game.ctx.moveTo(0, y * game.board.tileSize);
    game.ctx.lineTo(game.board.width * game.board.tileSize, y * game.board.tileSize);
  }

  game.ctx.strokeStyle = "black";
  game.ctx.stroke();
}

var renderEntities = function (game) {
  game.cities.map(function (city) { renderCity(game, city); });
  game.units.map(function (unit) { renderUnit(game, unit); });
}

var renderCity = function (game, city) {
  game.ctx.drawImage(
    game.images['city'],
    city.x * game.board.tileSize,
    city.y * game.board.tileSize,
    game.board.tileSize,
    game.board.tileSize
  );
}

var renderUnit = function (game, unit) {
  game.ctx.drawImage()
}

var render = function (game) {
  game.ctx.clearRect(0, 0, game.canvas.width, game.canvas.height);

  renderTerrain(game);
  renderGrid(game);
  renderEntities(game);
}

var gameLoop = function (game) {
  //game = update(game);
  render(game);
}

var initializePotato = function () {
  var game = {
    images: null,
    ctx: null,
    canvas: null,

    board: {
      width: 20,
      height: 20,

      tileSize: 40,

      tiles: []
    },

    cities: [],
    units: []
  };

  game.images = loadImages('img');
  game.board.tiles = makeTiles(game);

  game.canvas = $('<canvas/>').attr({
    width: game.board.width * game.board.tileSize,
    height: game.board.height * game.board.tileSize
  }).appendTo('body');

  game.ctx = game.canvas.get(0).getContext("2d");

  setInterval(function () { gameLoop(game); }, 1000);
};