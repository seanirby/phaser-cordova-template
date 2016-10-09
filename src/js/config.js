import Utils from './objects/Utils';

let clientWidth = function () {
    return Math.max(window.innerWidth, document.documentElement.clientWidth);
};
let clientHeight = function () {
    return Math.max(window.innerHeight, document.documentElement.clientHeight);
};


export default {
    init: function(){
        this.screen.width = window.innerWidth * window.devicePixelRatio;
        this.screen.height = window.innerHeight * window.devicePixelRatio;
        this.screen.offsetX = 0;
        this.screen.offsetY = 0;

        let desiredAspectRatio = 1.5;
        let actualAspectRatio = this.screen.width/this.screen.height;


        if(actualAspectRatio > desiredAspectRatio){
            this.screen.gameHeight = this.screen.height; this.screen.gameWidth = desiredAspectRatio * this.screen.gameHeight;
            this.screen.offsetX = (this.screen.width - this.screen.gameWidth)/ 2;
        } else {
            this.screen.gameWidth = this.screen.width;
            this.screen.gameHeight = this.screen.gameWidth / desiredAspectRatio;
            this.screen.offsetY = (this.screen.height - this.screen.gameHeight)/2;
        }

        this.screen.actualAspectRatio = actualAspectRatio;
        this.screen.desiredAspectRatio = desiredAspectRatio;

        this.buttonPadding = this.buttonPaddingScale * this.screen.gameHeight;
        this.screen.padding = this.paddingScale * this.screen.gameHeight;
    },
    init2(game){
        this.game = game;

        // TODO: the below is how you would scale your state backgrounds according ot screen size

        // //figure out tile scale
        // let testTile = new Phaser.Sprite(this.game, 0, 0, 'atlas', 'menu-bg');
        // let scaleBase = Utils.scaleTo(testTile, this.tileBgScale);
        // this.tileScale = scaleBase;
        // this.tileBaseHeight = this.screen.height/scaleBase;
        // this.tileBaseWidth = this.screen.width/scaleBase;

        // //figure out gameplayBgScale
        // testTile = new Phaser.Sprite(this.game, 0, 0, 'atlas', 'background');
        // scaleBase = config.screen.height/testTile.height;
        // this.gameplayBgScale = scaleBase;
        // this.gameplayBgBaseHeight = this.screen.height / scaleBase;
        // this.gameplayBgBaseWidth = this.screen.width / scaleBase;
// //figure out dialogTextScale
        // let dialogBounds = Utils.dialogBoxBounds();
        // let testText = new Phaser.BitmapText(game, 0, 0, 'main-font', '0');
        // this.dialogTextScale = this.dialogTextScale * (dialogBounds.bottom-dialogBounds.top)/testText.height;

    },
    debug: true,
    isCordova: typeof window.cordova !== 'undefined',
    colors: {
        white: "0xFFFFFF",
        black: "0x000000",
        red0: "0xFF0000",
        blue0: "0x0000FF",
        green0: "0x11ff6b",
        yellow0: "0xFFFF00",
        purple0: "0xd21acc"
    },
    dialog: {
        paddingLeft: 0
    },
    screen: {
        padding: 10
    }
};
