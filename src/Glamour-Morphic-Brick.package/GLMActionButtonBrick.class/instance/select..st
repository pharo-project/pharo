brick-interactable-actions
select: anEvent
	super select: anEvent.
	
	hoverAction ifNotNil: [ :a | a brickValue: self withEnoughArguments: { anEvent } ]