import config from '../config';

export default class Preload extends Phaser.State {
    constructor(){
        super();
        this.ready = false;
    }

    preload(){
        this.load.atlasXML('atlas', 'gfx/atlas.png', 'gfx/atlas.xml', Phaser.Loader.TEXTURE_ATLAS_XML_STARLING);

        //fonts
        this.load.bitmapFont('main-font', 'fonts/main-font.png', 'fonts/main-font.fnt');


        this.load.onLoadComplete.addOnce(()=>{
            console.log('assets loaded');
            this.ready = true;
        }, this);
    }

    create(){
    }

    update(){
        let nextState;

        if(!!this.ready) {
            config.init2(this.game);

            if(true){
                nextState = 'screen-test';
            } else {
                nextState = 'play';
            }

            this.game.state.start(nextState);
        }
    }
}
