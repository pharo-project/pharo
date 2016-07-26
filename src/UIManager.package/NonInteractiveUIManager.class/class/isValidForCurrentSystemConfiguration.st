testing
isValidForCurrentSystemConfiguration

	^ Smalltalk isHeadless and: [ Smalltalk isInteractive not ]