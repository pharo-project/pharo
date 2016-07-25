private
terminate: aContext
	| context |
	context := aContext.
	[context notNil] whileTrue: [context := context swapSender: nil]