generate
preamble: aNode
	^aNode isMessage 
		ifTrue: [ self preambleForMessage: aNode ]
		ifFalse: [ self preambleForMethod: aNode ]