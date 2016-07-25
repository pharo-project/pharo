drawing
bottomVisibleRowForCanvas: aCanvas
        "return the bottom visible row in aCanvas's clip rectangle"
        ^self rowAtLocation: aCanvas clipRect bottomLeft.
