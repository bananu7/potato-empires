
    'use strict';

function loadImages(prefix) {
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

    function makeTiles(game) {
      var tiles = new Array(game.board.width);

      for (var i = 0; i < game.board.width; ++i) {
        tiles[i] = new Array(game.board.height);
      }

      return tiles;
    }


    function drawBoard(game) {
        for (var y = 0; y < game.board.height; ++y) {
            for (var x = 0; x < game.board.width; ++x) {

            }   
        }

        for (var x = 0; x < game.board.width; ++x) {
            game.ctx.moveTo(x * game.board.tileSize, 0);
            game.ctx.lineTo(x * game.board.tileSize, game.board.height * game.board.tileSize);
        }

        for (var y = 0; y < game.board.height; ++y) {
            game.ctx.moveTo(0, y * game.board.height);
            game.ctx.lineTo(game.board.width * game.board.tileSize, y * game.board.tileSize);
        }

        game.ctx.strokeStyle = "black";
        game.ctx.stroke();
    }

    function render(game) {
      game.ctx.beginPath();
      game.ctx.clearRect(0, 0, game.canvas.width, game.canvas.height);

      drawBoard(game);
    }

    function gameLoop(game) {
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