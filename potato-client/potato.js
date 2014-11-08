'use strict';

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CONFIG
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var config = {
    serverUrl: "",
    updateInterval: 500
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

var renderText = function (game, x, y, text) {
  game.ctx.font = "30px Arial";
  game.ctx.fillStyle = 'white';
  game.ctx.fillText(text, x, y);
  game.ctx.lineWidth = 1;
  game.ctx.strokeStyle = "black";
  game.ctx.strokeText(text, x, y);
}

var renderTextOnTile = function (game, x, y, text) {
  renderText(
    game,
    x * game.board.tileSize + 2,
    y * game.board.tileSize + game.board.tileSize - 2,
    text
  );
}

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
  if (city.owner) {
    renderOwnerOverlay(game, city, city.owner);
  }
  renderTile(game, 'city', city.x, city.y);
};

var renderUnit = function (game, unit) {
  if (findCityAt(game, unit.x, unit.y) === null) {
    renderOwnerOverlay(game, unit, unit.owner);
  }
  renderTile(game, 'unit', unit.x, unit.y);
  renderTextOnTile(game, unit.x, unit.y, unit.value);
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

var renderOwnerOverlay = function (game, position, owner) {
  var red = 'rgba(255, 0, 0, 0.5)';
  var green = 'rgba(0, 255, 0, 0.5)';
  var blue = 'rgba(0, 0, 255, 0.5)';
  var violet = 'rgba(220, 0, 220, 0.5)';
  var white = 'rgba(255, 255, 255, 0.5)';
    
  var color = white;
    
  if (owner === "Redosia") { color = red; }
  else if (owner === "Greenland") { color = green; }
  else if (owner === "Bluegaria") { color = blue; }
  else if (owner === "Shitloadnam") { color = violet; }
  
  renderColorOverlay(game, position, color);
}

var renderColorOverlay = function (game, position, color) {
  game.ctx.beginPath();
  
  game.ctx.rect(
    position.x * game.board.tileSize,
    position.y * game.board.tileSize,
    game.board.tileSize,
    game.board.tileSize
  );

  game.ctx.fillStyle = color;
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
  getInitialState(game)
  .done(function (data) {
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
    })
    .bind('touchmove', function (event) {
      event.preventDefault();
      var touch = event.originalEvent.touches[0] || event.originalEvent.changedTouches[0];
      var elm = $(this).offset();
      var x = touch.pageX - elm.left;
      var y = touch.pageY - elm.top;
 
      event.pageX = x;
      event.pageY = y;

      updateMouse(game, event);
    });
      
    game.statusDisplay = $('<div/>')
      .appendTo('body');
    
    game.ctx = game.canvas.get(0).getContext('2d');

    // From then on just refresh units
    setInterval(function () { 
        updateGame(game);
    }, config.updateInterval);

    // Also start drawing
    setInterval(function () { render(game); }, 30)
  })
  .fail(function(error) {
      alert("Initial request failed");
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

      if (unit === null)
          break;
      
      if (unit.owner !== game.currentPlayer)
          break;
      
      game.selectionState = 'UNIT';
      game.selectedUnit = unit;
      
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

var updateGame = function (game) {
  $.get(config.serverUrl + '/update')
  .done(function (data) {
    game.units = data.units;
    game.cities = data.cities;
    game.currentPlayer = data.currentPlayer;
    game.movesLeft = data.movesLeft;
      
    game.statusDisplay.html(
        "CurrentPlayer: " + game.currentPlayer + "<br>"
       +"Moves left: " + game.movesLeft
    );
  });
};

var getInitialState = function(game) {
  return $.getJSON(config.serverUrl + "/initial", function (data) {
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

var findCityAt = function (game, x, y) {
  var result = $.grep(game.cities, function (city) {
    return city.x === x && city.y === y;
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
      
      // A non-move is an invalid move
      if (x === unit.x && y === unit.y) {
          continue;
      }

      possibleMoves.push({ x: x, y : y});
    }
  }
  return possibleMoves;
}

