import config from '../config';

class Bounds {
    constructor(top, right, bottom, left){
        this.top = top;
        this.right = right;
        this.bottom = bottom;
        this.left = left;
    }
}

export default {
    adjustX(displayObject){
        displayObject.x += -displayObject.width/2;
    },

    adjustY(displayObject){
        displayObject.y += -displayObject.height/2;
    },

    adjustXY(displayObject){
        this.adjustX(displayObject);
        this.adjustY(displayObject);
    },

    moveCentered(displayObject, x, y){
        displayObject.reset(x, y);
        this.adjustCenter(displayObject);

        return displayObject;
    },

    screenBounds(){
        let scr = config.screen;

        return new Bounds(0,
                          scr.width,
                          scr.height,
                          0);
    },

    gameBounds(){
        let scr = config.screen;
        let left = scr.offsetX;
        let top = scr.offsetY;

        return new Bounds(top,
                          left+scr.gameWidth,
                          top+scr.gameHeight,
                          left);
    },

    gamePaddedBounds(padding = config.screen.padding){
        let bounds = this.gameBounds();

        return new Bounds(bounds.top + padding,
                          bounds.right - padding,
                          bounds.bottom - padding,
                          bounds.left + padding);
    },

    dialogBoxBounds(){
        let scr = config.screen;
        return new Bounds((2/3)*scr.height,
                          scr.width,
                          scr.height,
                          0);
    },

    screenMiddle(){
        return new Phaser.Point(
            config.screen.offsetX + config.screen.gameWidth/2,
            config.screen.offsetY + config.screen.gameHeight/2
        );
    },

    scaleTo(displayObj, percentage, byWidth = false){
        let x, y, scaleFactor;

        if(byWidth){
            x = displayObj.width;
            y = config.screen.gameWidth;
        } else {
            x = displayObj.height;
            y = config.screen.gameHeight;
        }

        scaleFactor = (percentage * y)/x;
        displayObj.scale.setTo(scaleFactor);

        return scaleFactor;
    },

    scaleMultipleTo(displayObjs, percentage){
        displayObjs.forEach((displayObj)=>{
            this.scaleTo(displayObj, percentage);
        }, this);
    },

    // performs a relative move taking into account adjusted game dimensions
    moveX(displayObj, percentage){
        displayObj.x = percentage * config.screen.gameWidth + config.screen.offsetX;
    },

    // performs a relative move taking into account adjusted game dimensions
    moveY(displayObj, percentage){
        displayObj.y = percentage * config.screen.gameHeight + config.screen.offsetY;
    },

    effectiveWidth(displayObj, scale){
        scale = scale || displayObj.scale.x;
        return displayObj.width/scale;
    },

    effectiveHeight(displayObj, scale){
        scale = scale || displayObj.scale.y;
        return displayObj.height/scale;
    },

    //todo: bad method,
    makeRange(length){
        let out = [];
        let i = -1;

        for(; ++i < length;){
            out.push(i);
        }

        return out;
    }
};
