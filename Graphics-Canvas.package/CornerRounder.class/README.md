This class is a quick hack to support rounded corners in morphic.

Rather than produce rounded rectangles, it tweaks the display of corners.
Rather than work for any radius, it only supports a radius of 6.
Rather than work for any border width, it only supports widths 0, 1 and 2.
The corners, while apparently transparent, still behave opaquely to mouse clicks.

Worse than this, the approach relies on the ability to extract underlying bits from the canvas prior to display.  This ran afoul of top-down display, it seems, in SystemWindow spawnReframeHandle: (qv).  It will also make a postscript printer very unhappy.

But, hey, it's cute.