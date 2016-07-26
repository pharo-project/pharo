drawing
topVisibleRowForCanvas: aCanvas startingAt: aPos
     "return the top visible row in aCanvas's clip rectangle"

        ^ self rowAtLocation: (aCanvas clipRect topLeft) startingAt: aPos.
