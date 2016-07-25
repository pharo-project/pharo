private
msgClassComment: string with: chgRec

	| tokens theClass |
	tokens := self scannedSource: string.
	
	(tokens size >= 3
		and:[ tokens last isString ])
	ifTrue:[
		theClass := self classNamed: tokens first.
		((tokens at: (tokens size - 1)) == #class
		ifTrue: [ theClass theMetaClass ]
		ifFalse:[ theClass ]) comment: tokens last asString ]