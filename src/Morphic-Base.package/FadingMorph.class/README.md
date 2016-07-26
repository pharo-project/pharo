FadingMorph is a morph that fades out of existence after a certain time. The implementation uses a stepping event which reduces the visibility of the morph and its submorphs until they are no longer visible, when this happens the morph will delete itself.

Variables:
<alpha> holds the current alpha value. [ Float ]
<fadingFactor> a value of how much the alpha shall be reduced per step message send. [ Float ]
<resetable> boolean value used by #handleMouseMove: to descide whether or not to reset the alpha on mouse over.

The morph has a default layout, color, and borderstyle which can be changed after instance creation the same way other morphs can.

The morph itself updates its translucency per 100ms. One can change the variable fadingFactor to reduce or increase the time a FadingMorph is present in the world.

If the mouse is moved over a FadingMorph present in the world, it will reset the alpha, which means that the fading will start from the begining again, this can be turned off by sending #beUnResetable. 
