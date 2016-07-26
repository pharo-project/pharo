drawing
drawRawColorOn: aCanvas forSubmorph: aSubMorph
	| c frame |
	frame := (aSubMorph fullBounds withWidth: owner scroller innerBounds width) translateBy:(owner scroller offset x)@0.
	aSubMorph = owner listManager searchedElement
		ifTrue: [ 
			aCanvas fillRectangle: frame color: owner secondarySelectionColor.
			^ self ].
	owner listManager isCheckList
		ifTrue: [ 
			aSubMorph = owner listManager lastClickedMorph
				ifTrue: [ 
					aCanvas fillRectangle: frame color: owner selectionColorToUse.
					^ self ] ]
		ifFalse: [ 
			aSubMorph selected
				ifTrue: [ 
					aCanvas fillRectangle: frame color: owner selectionColorToUse.
					^ self ] ].
	(c := aSubMorph color) notNil
		ifTrue: [ 
			c isColor
				ifTrue: [ 
					aCanvas
						frameAndFillRectangle: frame
						fillColor: c
						borderWidth: 0
						borderColor: Color transparent ]
				ifFalse: [ 
					c origin: aSubMorph bounds topLeft.
					c direction: aSubMorph bounds width @ 0.
					aCanvas fillRectangle: frame basicFillStyle: c ] ]