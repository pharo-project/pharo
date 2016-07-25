services
runModal: aWidget
	
	[aWidget world notNil] whileTrue: [
		aWidget outermostWorldMorph doOneCycle]