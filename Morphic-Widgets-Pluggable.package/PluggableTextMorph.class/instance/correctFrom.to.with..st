interactive error protocol
correctFrom: start to: stop with: aString
	unstyledAcceptText ifNotNil: [unstyledAcceptText replaceFrom: start to: stop with: aString ].
	^ self handleEdit: [textMorph editor correctFrom: start to: stop with: aString]