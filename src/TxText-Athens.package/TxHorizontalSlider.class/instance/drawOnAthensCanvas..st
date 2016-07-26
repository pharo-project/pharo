drawing
drawOnAthensCanvas: aCanvas
	"before rendering transform from a global coordinates (Morphic) to relative coordinates (Athens)"
	| left right |
	left := bounds left + 1.
	right := bounds right - 1.
	aCanvas 
		setPaint: self backgroundColor;
		drawShape: bounds.
		
	aCanvas 
		setPaint: (captured ifTrue: [ self sliderHighlightColor ] ifFalse: [ self sliderColor] ).
		
	aCanvas drawShape: 
		((bounds left + mappedPosition max: left) @ (bounds top+1)
		rectangle: (bounds left + mappedPosition + mappedThickness min: right ) @ (bounds bottom-1)).
