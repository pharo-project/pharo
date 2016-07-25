drawing
setColumnWidthsFor: aCanvas
        | row topRow bottomRow |
        "set columnWidths for drawing on the specified canvas"
		columnWidths ifNil: [
		columnWidths := (self item: 1) collect: [ :ignored | 0 ]. ].
	topRow := (self topVisibleRowForCanvas: aCanvas) max: 1.
	bottomRow :=  (self bottomVisibleRowForCanvas: aCanvas) max: 1.
	topRow > bottomRow ifTrue: [ ^ self ].
	topRow to: bottomRow do: [ :rowIndex |
                row := self item: rowIndex.
                columnWidths := columnWidths with: row collect: [ :currentWidth :item |
				| widthOfItem |
				widthOfItem := (font widthOfStringOrText: item).
				widthOfItem > currentWidth
					ifTrue: [ self changed.  widthOfItem ]
					ifFalse: [ currentWidth ] ] ]