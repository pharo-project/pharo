accessing
completionToken
	completionToken
		ifNil:
			[ self isPositionAtPatternArg
				ifTrue: [ completionToken := self tokenForMethodDefinition ]
				ifFalse: [ completionToken := self tokenAtCurrentRange ] ].
	^ completionToken