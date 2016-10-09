import config from '../config';
import Boot from '../states/Boot';
import Play from '../states/Play';
import Preload from '../states/Preload';
import ScreenTest from '../states/ScreenTest';

export default class Game extends Phaser.Game {
	  constructor() {
		    super(config.screen.width, config.screen.height, Phaser.AUTO, 'game', null);
		    this.state.add('play', Play, false);
		    this.state.add('screen-test', ScreenTest, false);
		    this.state.add('preload', Preload, false);
		    this.state.add('boot', Boot, true);
    }
}
