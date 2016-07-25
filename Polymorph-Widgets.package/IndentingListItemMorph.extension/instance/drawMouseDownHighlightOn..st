*Polymorph-Widgets
drawMouseDownHighlightOn: aCanvas
	"Draw with a dotted border."
	
	|frame|
	self highlightedForMouseDown ifTrue: [
		container ifNil: [^super drawMouseDownHighlightOn: aCanvas].
		frame := self selectionFrame.
		aCanvas 
			frameRectangle: frame
			width: 1
			colors: {container mouseDownHighlightColor. Color transparent}
			 dashes: #(1 1)]