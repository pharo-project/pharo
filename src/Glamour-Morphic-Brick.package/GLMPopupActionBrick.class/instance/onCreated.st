initialization
onCreated
	| theContentBricks |
	
	theContentBricks := onCreatedBlock brickValue: self withEnoughArguments: { self triggerBrick }.
	theContentBricks := theContentBricks asOrderedCollection select: #isNotNil.
	self contentBrick addAllBricks: theContentBricks