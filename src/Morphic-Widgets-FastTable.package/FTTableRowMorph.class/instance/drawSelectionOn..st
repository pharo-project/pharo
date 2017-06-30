drawing
drawSelectionOn: aCanvas

	| clr |
		
	clr := over == true ifTrue: [ 
		selectionColor isTransparent 
			ifTrue: [ self mouseOverColor ]
			ifFalse: [ self mouseOverColor mixed: 0.75 with: selectionColor ]
	] 
	ifFalse: [ selectionColor ].
	
	clr isTransparent ifTrue: [ ^ self ].
	aCanvas fillRectangle: self bounds 
		color: (clr alpha: 0.2)