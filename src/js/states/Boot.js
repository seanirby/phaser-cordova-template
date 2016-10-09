export default class Preload extends Phaser.State {
    constructor(){
        super();
    }

    preload(){
        // TODO: Add loading animation here
    }

    create(){
		    this.game.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
		    this.game.scale.pageAlignHorizontally = true;
		    this.game.scale.pageAlignVertically = true;
        this.game.scale.refresh();

        this.game.input.maxPointers = 1;
        this.game.state.start('preload');
    }
}
