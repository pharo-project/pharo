brick-interactable-actions
click: anEvent
	
	clickAction ifNil: [ ^ false ].
	clickAction brickValue: self withEnoughArguments: { anEvent }.
	
	^ true