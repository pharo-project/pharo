testing
isSelector: aSymbol 
	| scanner token |
	scanner := self basicNew.
	scanner on: (ReadStream on: aSymbol asString).
	scanner step.
	token := scanner scanAnySymbol.
	token isLiteralToken ifFalse: [^false].
	token value isEmpty ifTrue: [^false].
	^scanner atEnd