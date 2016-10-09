# Phaser Cordova Template

This repo contains a project template for building a multiplatform
mobile game with Phaser and Apache Cordova.  This documentation is
mainly for my purposes of remembering but it should be useful if you
want to start a project with these technologies.

This project was based off of the [phaser-es6-boilerplate](https://github.com/belohlavek/phaser-es6-boilerplate)

## Features

1. Gulp script for autobuilding, running unit tests, and local project hosting
2. Karma unit tests
3. Module support using browserify
4. ES6 Support using babelify
5. Source maps
6. Texture atlas and texture atlas export script for GIMP
7. Auto display sizing to your game's desired aspect ratio

# Example Setup

```
cd ~/dev
git clone git@github.com:seanirby/phaser-cordova-template.git
cd phaser-cordova-template
npm update
npm install
cordova platform add ios
cordova platform add android
```

To start the dev autobuild/test process, you need to run the following from the
project directory.

```
npm start
```

# Building for mobile
Before you can build the iOS mobile executable you must edit `build.json` to provide your development team id.  This can be found in your developer portal.  Once that is done, run the following to start your project:

```
cordova build ios
cordova run ios
```

# Project Structure

## asset_projects
Contains assets that may need some processing before being placed in
the `static` folder.

For instance, `asset_projects/gfx` contains a GIMP
project named `atlas` where you can draw your game's graphics.  Each
layer in this file corresponds to a game sprite. Export the atlas to
the `static` folder by running the gimp script in
`gimp_scripts/make_atlas_files.scm`.

## build.json
Cordova specific. See cordova docs

## config.xml
Cordova specific. See cordova docs

## gulpfile.js
This file defines a script that builds your project, autobuilds on
changes, and runs your unit tests.  The project is compiled from the
content in `src`, `static`, `node_modules/phaser`, and
`node_modules/javascript-state-machine`. Tests are run found in
`tests` folder.

The compiled assets can be found in `www`.  See script for details.

## hooks
Cordova specific. See cordova docs

## info.txt
Cordova specific. See cordova docs

## karma.conf.js
Unit test configuration

## package.json
npm configuration file

## platforms
Cordova specific. See cordova docs

## plugins
Cordova specific. See cordova docs

## server.rb
Helper file for starting up a local web server serving content in the
`www` directory.  This is useful if you need to send the game to an
artist who needs to test the game but may not be technical.  All they
need to do is run the following command from the project folder.

```
ruby server.rb
```

You should be able to see the project on `http://localhost:8001`.

## src
Folder for source files that need some form of compilation.  Currently
it contains the project's JS files and `index.html`.  The javascript
files in this folder support ES6 syntax and will be compiled into
`www/game.js` after building.

The html file in this folder will be
compiled into `www/index.html` after building.  The html file is
preprocessed in the gulpfile so you can conditionally place content
in `index.html` if you wish.

## static
Project assets such as sound files, images, and stylesheets.  These
files are simply copied into the `www` directory when built.

## test
Directory for storing your Karma unit tests

## www
Project build folder.

# Code Structure

Our application code starts in `src/js/index.js` with the call to `app.initialize()`.  This sets up the event callback we need to start our game.  For desktop/non-mobile users, we listen for the `DOMContentLoaded` event.  For mobile users we listen for the `deviceready` event.  These callbacks will perform any context specific setup and then start the game.

The game states can be found in `src/js/states`.  This template is set to execute the following sequence of states on startup.

1. Boot
2. Preload
3. ScreenTest

The `ScreenTest` state displays information about your window dimensions and device dimensions.  Once you start development on your game, you will want to edit the `Preload` state so you don't immediately go to the `ScreenTest`.
