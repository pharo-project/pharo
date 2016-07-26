as yet unclassified
layoutProportional: aCollection in: aBrick position: aSymbol
	| fullWidth eachWidth index |
	aCollection isEmpty ifTrue: [ ^ self ].
	
	fullWidth := aBrick innerBounds width.
	index := 0.
	aCollection inject: aBrick paddingLeft into: [ :leftOffset :each |
		|layoutBounds|
		
		self sendEvent: (GLMBrickLayoutStacked new index: (index + 1); position: aSymbol; yourself) to: each.
		
		eachWidth := ((fullWidth - leftOffset) / (aCollection size - index)) ceiling.
		
		layoutBounds := each wrappedBounds.
		index = 0
			ifTrue: [ layoutBounds width: (((each layout maxWidth brickValue: each) max: eachWidth) min: (fullWidth - leftOffset)) ]
			ifFalse: [				
				(index = (aCollection size - 1))
					ifFalse: [ layoutBounds width: (eachWidth min: (each layout maxWidth brickValue: each)) ]
					ifTrue: [ layoutBounds width: eachWidth ] ].
		
		layoutBounds translateBy: (leftOffset@0).
		each brickBounds: layoutBounds.
		index := index + 1.
		leftOffset + layoutBounds outerBounds width ]