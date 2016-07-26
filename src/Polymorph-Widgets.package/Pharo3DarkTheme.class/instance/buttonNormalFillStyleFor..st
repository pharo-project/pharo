fill-styles-buttons
buttonNormalFillStyleFor: aButton
	"Return the normal button fillStyle for the given button."
	
	(aButton valueOfProperty: #noFill ifAbsent: [false]) 
		ifTrue: [ ^ SolidFillStyle color: Color transparent ].
		
	^ SolidFillStyle color: self buttonColor.