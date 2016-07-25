do-its
selectionString
	"remove the initial and final quote marks, if present"
	"'''h''' withoutWrappingDoubleQuotes"
	| quote selection  |
	selection :=  self selection string trimBoth .
	selection size < 2 ifTrue: [ ^ selection ].
	quote := selection first.
	^ quote = $"
		ifTrue: [ selection copyFrom: 2 to: selection size - 1 ]
		ifFalse: [ selection ].