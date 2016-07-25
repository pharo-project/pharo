brick-interactable-actions
deselect: anEvent
	super deselect: anEvent.
	
	unhoverAction ifNotNil: [ :a | a brickValue: self withEnoughArguments: { anEvent } ]