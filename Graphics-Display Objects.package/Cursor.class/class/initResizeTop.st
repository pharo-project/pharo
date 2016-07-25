initialization - private
initResizeTop
    "Cursor initResizeTop"
       ResizeTopCursor :=
               (Cursor extent: 16 @ 16 fromArray: #(
               2r000000100000000
               2r000001110000000
               2r000011111000000
               2r000111111100000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000111111100000
               2r000011111000000
               2r000001110000000
               2r000000100000000
               2r000000000000000)
       offset: -7 @ -7) withMask