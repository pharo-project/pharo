drawing
drawOnAthensCanvas: aCanvas
	"before rendering transform from a global coordinates (Morphic) to relative coordinates (Athens)"
	| top bottom |
	
	top := bounds top + 1.
	bottom := bounds bottom - 1.
	
	aCanvas 
		setPaint: self backgroundColor;
		drawShape: bounds.
		
		
	aCanvas 
		setPaint: (captured ifTrue: [ self sliderHighlightColor ] ifFalse: [ self sliderColor] ).
		
	 aCanvas drawShape: 
		(bounds left + 1 @ (bounds top + mappedPosition max: top ) 
		rectangle: bounds right -1 @ (bounds top + mappedPosition + mappedThickness min: bottom ))  
	 