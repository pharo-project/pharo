running-helpers
checkIsArgumentDefinition: aNode

	^ aNode isArgument and: [ aNode parent isMethod ]