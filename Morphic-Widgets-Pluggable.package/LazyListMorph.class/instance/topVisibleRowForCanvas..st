drawing
topVisibleRowForCanvas: aCanvas
        "return the top visible row in aCanvas's clip rectangle"
        ^self rowAtLocation: aCanvas clipRect topLeft.
