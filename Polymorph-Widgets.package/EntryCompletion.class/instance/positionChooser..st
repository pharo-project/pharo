accessing
positionChooser: aChooser
        "Position the list morph to fit on the display."

        aChooser boundsInWorld bottomLeft y + aChooser listHeight > aChooser world bottom
                ifTrue: [aChooser listMorph
                                        bounds: (aChooser boundsInWorld topLeft - (0 @ aChooser listHeight) extent: aChooser width @ aChooser listHeight)]
                ifFalse: [aChooser listMorph
                                        bounds: (aChooser boundsInWorld bottomLeft extent: aChooser width @ aChooser listHeight)] 