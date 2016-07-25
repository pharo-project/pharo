keyboard managing
arrowEvent: event key: aChar target: aMorph
	"Transmit flow to next column"
	^ self model keyPressedOnTree: event target: aMorph.