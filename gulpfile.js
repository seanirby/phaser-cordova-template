var del = require('del');
var gulp = require('gulp');
var path = require('path');
var argv = require('yargs').argv;
var gutil = require('gulp-util');
var source = require('vinyl-source-stream');
var buffer = require('gulp-buffer');
var uglify = require('gulp-uglify');
var gulpif = require('gulp-if');
var preprocess = require('gulp-preprocess');
var exorcist = require('exorcist');
var babelify = require('babelify');
var browserify = require('browserify');
var browserSync = require('browser-sync');
var server = require('karma').Server;

/**
 * LIB PATHS
 */
var PHASER_PATH = './node_modules/phaser/build/';
var STATE_MACHINE_PATH = './node_modules/javascript-state-machine/';

var BUILD_PATH = './www';
var SCRIPTS_PATH = BUILD_PATH + '/js';
var SOURCE_PATH = './src';
var JS_PATH = SOURCE_PATH + '/js';
var HTML_PATH = SOURCE_PATH + '/html';
var STATIC_PATH = './static';
var ENTRY_FILE = JS_PATH + '/index.js';
var OUTPUT_FILE = 'game.js';
var INDEX = 'index.html';

var keepFiles = false;

/**
 * Simple way to check for development/production mode.
 */
function isProduction() {
    return argv.production;
}

/**
 * Logs the current build mode on the console.
 */
function logBuildMode() {
    
    if (isProduction()) {
        gutil.log(gutil.colors.green('Running production build...'));
    } else {
        gutil.log(gutil.colors.yellow('Running development build...'));
    }

}

/**
 * Deletes all content inside the './build' folder.
 * If 'keepFiles' is true, no files will be deleted. This is a dirty workaround since we can't have
 * optional task dependencies :(
 * Note: keepFiles is set to true by gulp.watch (see serve()) and reseted here to avoid conflicts.
 */
function cleanBuild() {
    if (!keepFiles) {
        del([BUILD_PATH+'/**/*.*']);
        //del(['build/**/*.*']);
    } else {
        keepFiles = false;
    }
}

/**
 * Copies the content of the './static' folder into the '/build' folder.
 * Check out README.md for more info on the '/static' folder.
 */
function copyStatic() {
    return gulp.src(STATIC_PATH + '/**/*')
        .pipe(gulp.dest(BUILD_PATH));
}

function buildHtml() {
    gulp.src(SOURCE_PATH + '/html/index.html')
        .pipe(preprocess({context: { IS_PRODUCTION: isProduction()}}))
        .pipe(gulp.dest(BUILD_PATH));
}

/**
 * Copies required library files from the './node_modules/' folder into the './www/js' folder.
 * This way you can call 'npm update', get the lastest Phaser version and use it on your project with ease.
 */
function copyLibs() {

    var srcList = [PHASER_PATH + 'phaser.min.js',
                   STATE_MACHINE_PATH + 'state-machine.min.js'];
    
    if (!isProduction()) {
        srcList.push(PHASER_PATH + 'phaser.map');
    }
    
    return gulp.src(srcList)
        .pipe(gulp.dest(SCRIPTS_PATH));

}


/**
 * Transforms ES2015 code into ES5 code.
 * Optionally: Creates a sourcemap file 'game.js.map' for debugging.
 * 
 * In order to avoid copying Phaser and Static files on each build,
 * I've abstracted the build logic into a separate function. This way
 * two different tasks (build and fastBuild) can use the same logic
 * but have different task dependencies.
 */
function build() {

    var sourcemapPath = SCRIPTS_PATH + '/' + OUTPUT_FILE + '.map';
    logBuildMode();

    return browserify({
        paths: [path.join(__dirname, "node_modules"),path.join(__dirname, 'src/js')],
        entries: ENTRY_FILE,
        debug: true,
        transform: [
            [
                babelify, {
                    presets: ["es2015"]
                }
            ]
        ]
    })
        .transform(babelify)
        .bundle().on('error', function(error) {
            gutil.log(gutil.colors.red('[Build Error]', error.message));
            this.emit('end');
        })
        .pipe(gulpif(!isProduction(), exorcist(sourcemapPath)))
        .pipe(source(OUTPUT_FILE))
        .pipe(buffer())
        .pipe(gulpif(isProduction(), uglify()))
        .pipe(gulp.dest(SCRIPTS_PATH));

}

/**
 * Starts the Browsersync server.
 * Watches for file changes in the 'src' folder.
 */
function serve() {
    
    var options = {
        server: {
            baseDir: BUILD_PATH,
            index: INDEX
        },
        open: false // Change it to true if you wish to allow Browsersync to open a browser window.
    };
    
    browserSync(options);
    
    // Watches for changes in files inside the './src' folder.
    gulp.watch(JS_PATH + '/**/*.js', ['watch-js']);
    
    // Watches for changes in files inside the './static' folder. Also sets 'keepFiles' to true (see cleanBuild()).
    gulp.watch(STATIC_PATH + '/**/*', ['watch-static']).on('change', function() {
        keepFiles = true;
    });


    gulp.watch(HTML_PATH + '/html/index.html', ['watch-html']).on('change', function() {
        keepFiles = true;
    });
}


gulp.task('tdd', function (done) {
    new server({
        configFile: __dirname + '/karma.conf.js',
        singleRun: false
    }, done).start();
});

gulp.task('cleanBuild', cleanBuild);
gulp.task('copyStatic', ['cleanBuild'], copyStatic);
gulp.task('buildHtml', ['copyStatic'], buildHtml);
gulp.task('copyLibs', ['buildHtml'], copyLibs);
gulp.task('build', ['copyLibs'], build);
gulp.task('fastBuild', build);
gulp.task('serve', ['build'], serve);
gulp.task('watch-html', ['buildHtml'], browserSync.reload);
gulp.task('watch-js', ['fastBuild'], browserSync.reload);
gulp.task('watch-static', ['copyLibs'], browserSync.reload);

gulp.task('default', ['serve', 'tdd']);
