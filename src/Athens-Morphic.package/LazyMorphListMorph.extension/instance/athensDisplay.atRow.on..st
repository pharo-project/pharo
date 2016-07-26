*Athens-Morphic
athensDisplay: item atRow: row on: aCanvas
	"Display the given item at the given row on the given canvas."

	row = self mouseDownRow
		ifTrue: [ 
			(aCanvas setStrokePaint: (self colorForRow: row))
				width: 1;
				dashes: {1} offset: 0.
			aCanvas drawShape: (self selectionFrameForRow: row) ]