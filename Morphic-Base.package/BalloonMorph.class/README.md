A balloon with text used for the display of explanatory information.

Balloon help is integrated into Morphic as follows:
If a Morph has the property #balloonText, then it will respond to #showBalloon by adding a text balloon to the world, and to #deleteBalloon by removing the balloon.

Moreover, if mouseOverEnabled is true (see class msg), then the Hand will arrange to cause display of the balloon after the mouse has lingered over the morph for a while, and removal of the balloon when the mouse leaves the bounds of that morph.  In any case, the Hand will attempt to remove any such balloons before handling mouseDown events, or displaying other balloons.

Balloons should not be duplicated with veryDeepCopy unless their target is also duplicated at the same time.