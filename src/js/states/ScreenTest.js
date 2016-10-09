import Utils from '../objects/Utils';
import config from '../config';

export default class ScreenTest extends Phaser.State {
    constructor(){
        super();
    }

    init(){

    }

    create(){
        let middle = Utils.screenMiddle();
        let screen = config.screen;
        let img = this.game.add.image(0, 0, 'atlas', 'screen-test-2');

        Utils.scaleTo(img, 1);
        img.reset(config.screen.offsetX, config.screen.offsetY);
        let scr = config.screen;

        let debugItems = {
            actualAspectRatio: scr.actualAspectRatio,
            desiredAspectRatio: scr.desiredAspectRatio,
            effectiveAspectRatio: scr.gameWidth/scr.gameHeight,
            offsetX: scr.offsetX,
            offsetY: scr.offsetY,
            windowWidth: window.innerWidth,
            windowHeight: window.innerHeight,
            devicePixelRatio: window.devicePixelRatio,
            effectiveWidth: scr.width,
            effectiveHeight: scr.height
        };

        let pos = {x: scr.offsetX, y: scr.offsetY};
        let yOffset = 20;
        let scaleFactor;

        for(let key in debugItems){
            let text = new Phaser.BitmapText(this.game, pos.x, pos.y, 'main-font',`${key}: ${debugItems[key]}`);
            text.tint = config.colors.black;

            if(!scaleFactor){
                Utils.scaleTo(text, .05);
                scaleFactor = text.scale;
            } else {
                text.scale.setTo(scaleFactor.x);
            }

            this.game.add.existing(text);
            pos.y += scaleFactor.x*yOffset + text.getBounds().bottom;
        }
    }
}
