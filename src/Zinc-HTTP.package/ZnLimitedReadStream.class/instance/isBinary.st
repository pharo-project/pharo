testing
isBinary
	^ binary isNil
		ifTrue: [ stream isBinary ]
		ifFalse: [ binary ]