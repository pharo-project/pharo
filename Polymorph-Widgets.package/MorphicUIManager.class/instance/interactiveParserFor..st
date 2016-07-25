settings
interactiveParserFor: requestor
	"during Morphic loading the interactive parser must be disabled"

	(interactiveParser = false) ifTrue: [ ^ false ].  "can be nil"
	^super interactiveParserFor: requestor