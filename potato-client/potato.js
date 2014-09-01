'use strict';

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CONFIG
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var config = {
    serverUrl: "http://localhost:3000",
    updateInterval: 1000
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RESOURCE LOADING AND GENERATION
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Loads the images from a given resource folder
 */
var loadImages = function (folder) {
  var images = {
    'grass': null,
    'water': null,
    'city': null,
    'unit': null
  };

  for (var i in images) {
    images[i] = new Image();
    images[i].src = folder + '/' + i + '.png';
  }

  return images;
};

/**
 * Generates a dump random map
 */
var makeTiles = function (game) {
  var tiles = new Array(game.board.width);

  for (var i = 0; i < game.board.height; ++i) {
    tiles[i] = new Array(game.board.width);
  }

  for (var y = 0; y < game.board.height; ++y) {
    for (var x = 0; x < game.board.width; ++x) {
      // So random
      var terrainType = Math.random() < 0.75 ? 'grass' : 'water';
      tiles[y][x] = terrainType;
    }
  }

  return tiles;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RENDER
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var render = function (game) {
  game.ctx.clearRect(0, 0, game.canvas.width, game.canvas.height);

  renderTerrain(game);
  renderGrid(game);
  renderEntities(game);

  renderInteractions(game);
};

var renderTerrain = function (game) {
  for (var y = 0; y < game.board.height; ++y) {
    for (var x = 0; x < game.board.width; ++x) {
      renderTile(game, game.board.tiles[y][x], x, y);
    }   
  }
};

/**
 * Draw a tile of a given type at the given map coordinates
 */
var renderTile = function (game, image, x, y) {
  game.ctx.drawImage(
    game.images[image],
    x * game.board.tileSize,
    y * game.board.tileSize,
    game.board.tileSize,
    game.board.tileSize
  );
};

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

  game.ctx.strokeStyle = 'black';
  game.ctx.stroke();
};

var renderEntities = function (game) {
  game.cities.map(function (city) { renderCity(game, city); });
  game.units.map(function (unit) { renderUnit(game, unit); });
};

var renderCity = function (game, city) {
  renderTile(game, 'city', city.x, city.y);
};

var renderUnit = function (game, unit) {
  renderTile(game, 'unit', unit.x, unit.y);
  // pick different image depending on player
  // or tint
};

var renderInteractions = function (game) {
  switch (game.selectionState) {
    case 'FREE':
      renderUnitPicker(game);
    break;
    case 'UNIT':
      renderSelectedUnit(game);
    break;
  }
};

var renderUnitPicker = function (game) {
  game.ctx.beginPath();
  game.ctx.rect(
    game.mouse.x * game.board.tileSize,
    game.mouse.y * game.board.tileSize,
    game.board.tileSize,
    game.board.tileSize
  );
  game.ctx.fillStyle = 'rgba(255, 255, 255, 0.5)';
  game.ctx.fill();
};

var renderSelectedUnit = function (game) {
  var unit = game.selectedUnit;
  // Render some kind of outline
  game.ctx.beginPath();
  game.ctx.rect(
    unit.x * game.board.tileSize,
    unit.y * game.board.tileSize,
    game.board.tileSize,
    game.board.tileSize
  );

  game.ctx.strokeStyle = 'red';
  game.ctx.stroke();

  // And the area where the unit may move
  // 3-block radius that is not water
  game.ctx.beginPath();

  findPossibleMoves(game, unit).forEach(function(point) {
    game.ctx.rect(
      point.x * game.board.tileSize,
      point.y * game.board.tileSize,
      game.board.tileSize,
      game.board.tileSize
    );
  });

  game.ctx.fillStyle = 'rgba(255, 255, 255, 0.5)';
  game.ctx.fill();
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// LOGIC AND STUFF
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var initializePotato = function () {
  var game = {
    images: null,
    ctx: null,
    canvas: null,

    mouse: {
      x: 0,
      y: 0
    },

    board: {
      width: 1,
      height: 1,

      tileSize: 40,

      tiles: []
    },

    cities: [],
    units: [],

    selectionState: 'FREE',
    selectedUnit: null
  };

  game.images = loadImages('img');
  //game.board.tiles = makeTiles(game);
    
  // Load first frame
  getInitialState(game).done(function (data) {
    game.canvas = $('<canvas/>').attr({
      width: game.board.width * game.board.tileSize,
      height: game.board.height * game.board.tileSize
    })
    .appendTo('body')
    .mousemove(function (event) {
      updateMouse(game, event);
    })
    .click(function (event) {
      handleClick(game);
    });
    
    game.ctx = game.canvas.get(0).getContext('2d');

    // From then on just refresh units
    setInterval(function () { updateUnits(game); }, config.updateInterval);

    // Also start drawing
    setInterval(function () { render(game); }, 30)
  });
};

var updateMouse = function (game, event) {
  game.mouse.x = Math.floor(event.pageX / game.board.tileSize);
  game.mouse.y = Math.floor(event.pageY / game.board.tileSize);
};

var handleClick = function (game, event) {
  switch (game.selectionState) {
    case 'FREE':
      // this is a selection attempt
      // find unit at coords
      // if any, mark as selected
      var unit = findUnitAt(game, game.mouse.x, game.mouse.y);

      if (unit !== null) {
        game.selectionState = 'UNIT';
        game.selectedUnit = unit;
      }
      break;
    case 'UNIT':
      // this is a move order
      // issue move order to server (modulo checks?)
      // can also update interaction renders
        
      var deselectUnit = function(game) { 
          game.selectionState = 'FREE';
          game.selectedUnit = null;
      };
      var possibleMoves = findPossibleMoves(game, game.selectedUnit);
      
      var compareByXY = function(a,b) { return a.x === b.x && a.y === b.y; };
      var isEqualToMouse = function(p) { return compareByXY(p, game.mouse); };
        
      if (possibleMoves.filter(isEqualToMouse).length === 0) {
          deselectUnit(game);
          break;
      }
      
      var moveData = {
          from: { x: game.selectedUnit.x, y: game.selectedUnit.y },
          to: { x: game.mouse.x, y: game.mouse.y }
      };

      $.post(config.serverUrl + '/move', JSON.stringify(moveData));
        
      deselectUnit(game);
      break;
  }
}

var updateUnits = function (game) {
  $.get(config.serverUrl + '/units', function (data) {
    game.units = data.units;
  });
};

var getInitialState = function(game) {
  return $.getJSON(config.serverUrl, function (data) {
    console.log(data);
    game.units = data.units;
    game.cities = data.cities;
    game.board.width = data.map[0].length;
    game.board.height = data.map.length;
    game.board.tiles = data.map;
  });
};

var findUnitAt = function (game, x, y) {
  var result = $.grep(game.units, function (unit) {
    return unit.x === x && unit.y === y;
  });

  return result.length === 0 ? null : result[0];
};

var findPossibleMoves = function (game, unit) {
  var possibleMoves = [];
  for (var y = unit.y - 1; y <= unit.y + 1; ++y) {
    // Bounds check
    if (y < 0 || y >= game.board.height) {
      continue;
    }

    for (var x = unit.x - 1; x <= unit.x + 1; ++x) {
      // Bounds check
      if (x < 0 || x >= game.board.width) {
        continue;
      }

      // Skip water
      if (game.board.tiles[y][x] === 'water') {
        continue;
      }

      possibleMoves.push({ x: x, y : y});
    }
  }
  return possibleMoves;
}

