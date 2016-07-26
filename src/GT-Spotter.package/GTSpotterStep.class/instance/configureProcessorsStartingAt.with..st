processing
configureProcessorsStartingAt: anIndex with: aConfigurationBlock
	"Execute aConfigurationBlock with all processors starting at anIndex.
	(this assumes processors are stored in an OrderedCollection that captures the
	orderin which they are added to the step.)"
	
	(anIndex > self processors size) ifTrue: [ ^ self ].
	anIndex to: self processors size do: [ :processorIndex |
		aConfigurationBlock value: (self processors at: processorIndex) ]