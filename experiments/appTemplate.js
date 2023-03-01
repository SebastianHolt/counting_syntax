// This is your app.js, the script that will handle requests to the experiments on your server


const { isUndefined } = require('lodash');
const { createWebSocketStream } = require('ws');

global.__base = __dirname + '/';

var
    use_https     = true,
    argv          = require('minimist')(process.argv.slice(2)),
    https         = require('https'),
    fs            = require('fs'),
    app           = require('express')(),
    _             = require('lodash'),
    parser        = require('xmldom').DOMParser,
    XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest,
    sendPostRequest = require('request').post;
    

////////// EXPERIMENT GLOBAL PARAMS //////////

var gameport;

if(argv.gameport) {
  gameport = argv.gameport;
  console.log('using port ' + gameport);
} else {
  gameport = 8887;
  console.log('no gameport specified: using 8887\nUse the --gameport flag to change');
}

try {
  var privateKey  = fs.readFileSync('/etc/letsencrypt/live/cogtoolslab.org/privkey.pem'),
      certificate = fs.readFileSync('/etc/letsencrypt/live/cogtoolslab.org/cert.pem'),
      intermed    = fs.readFileSync('/etc/letsencrypt/live/cogtoolslab.org/chain.pem'),
      options     = {key: privateKey, cert: certificate, ca: intermed},
      server      = require('https').createServer(options,app).listen(gameport),
      io          = require('socket.io')(server);
} catch (err) {
  console.log("cannot find SSL certificates; falling back to http");
  var server      = app.listen(gameport),
      io          = require('socket.io')(server);
}

// serve stuff that the client requests
app.get('/*', (req, res) => {
  serveFile(req, res);
});

// here's the list of games that are active
var gamesList = {};
var game_count = 0;
var connectionMap = {};

// there will be something in their client-side code that makes this connection
io.on('connection', function (socket) {

  // Recover query string information and set condition
  var referer = require('url').parse(socket.request.headers.referer, true);
  var query = referer.query;  // get Prolific ID info from here
  var pathname = referer.pathname;
  var path = referer.path;
  var href = referer.href;

  socket.emit('onConnected');

  // handle the case where it is a game that has just connected (not monitor or something)
  socket.on("gameConnect",function(){
    var exp = 'survive';

    // anyone logging on needs to see if there is already a game they can join
    socket.on('findSpot', function(){
      searching = 1;
      for (const [gameID, gameObject] of Object.entries(gamesList)){
        // console.log(gameObject.humans < gameObject.humans_max)
        if (gameObject.humans < gameObject.humans_max) {  // if we find an open game
          gid = gameID;
          g = gameObject;
          searching = 0;
          console.log("Found a game! Joining it")
          break; }

      }
      if(searching == 1){    // if we didn't find a game for them to join
        var newID = UUID();
        var game = new require([__base, exp, 'game.core.js'].join('/')).game_core({server: true, gameID: newID});
        gid = newID;
        g = game;

        gamesList[newID] = game;
        // socket.emit('updateGame',[newID, 'create']);
        game_count++;
        console.log("New game. How many games now? ",game_count);
      }
      // the following gets broadcast whether we created a new game or are joining an old one
      socket.emit('add2game',[gid,g])
    }
    ) // end of 'findSpot' listener
  
    // write data to db upon getting current data
    socket.on('currentData', function(data) {
      writeDataToMongo(data);
    });
  
    // write data to db upon getting current data
    socket.on('updateGame', function(data) {
      // console.log(data);
      
      gamesList[data[0]].change(data) // make the change according to the function defined in game.core
      console.log(gamesList[data[0]].players)
      
      // for (player in gamesList[data[0]].players){
      //   player.playerSocket.emit('updateClientGame',[data,gamesList[data[0]]])
      // }
      
      // need to replace this with EVERY socket/client in that game
      socket.emit('updateClientGame',[data,gamesList[data[0]]])
      
    });  

    
    socket.on('addPlayer2game',function(data){
      // console.log(socket)
      // gamesList[data[0]].add_player(data[1],0)
      connectionMap[connectionMap.length] = socket;
      console.log(connectionMap[connectionMap.length])
      connectionMap[connectionMap.length].emit('printThis',5)
      // for (connection in connectionMap){
      //   connection.emit('printThis',5)
      //   console.log(connection[0])
      // }

    });

  });

});

FORBIDDEN_FILES = ["auth.json"]
var serveFile = function(req, res) {
  var fileName = req.params[0];
  if(FORBIDDEN_FILES.includes(fileName)){
    // Don't serve files that contain secrets
    console.log("Forbidden file requested:" + filename);
    return; 
  }
  console.log('\t :: Express :: file requested: ' + fileName);
  return res.sendFile(fileName, {root: __dirname});
};

var UUID = function() {
  var baseName = (Math.floor(Math.random() * 10) + '' +
        Math.floor(Math.random() * 10) + '' +
        Math.floor(Math.random() * 10) + '' +
        Math.floor(Math.random() * 10));
  var template = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx';
  var id = baseName + '-' + template.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
  });
  return id;
};

var writeDataToMongo = function(data) {
  // console.log(data)
  sendPostRequest(
    'http://localhost:8980/db/insert',
    { json: data },
    (error, res, body) => {
      if (!error && res.statusCode === 200) {
        console.log(`sent data to store`);
      } else {
	      console.log(`error sending data to store: ${error} ${body}`);
      }
    }
  );
};