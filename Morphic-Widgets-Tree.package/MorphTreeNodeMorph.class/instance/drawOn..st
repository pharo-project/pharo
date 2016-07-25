drawing
drawOn: aCanvas
	"Note that selection is rendered from the container transformMorph (see MorphTreeTransformMorph)" 
	
	container withHLines 
		ifTrue: [
			aCanvas 
				frameRectangle: self selectionFrame
				width: 1
				colors: {Color veryLightGray. Color transparent}
				 dashes: #(1 2)].

	self hasToggle 
		ifTrue: [self drawToggleOn: aCanvas in: self toggleRectangle]. 
		
	container listManager isCheckList
		ifTrue: [self drawCheckOn: aCanvas ].
	

		
		
		
		
		
		
