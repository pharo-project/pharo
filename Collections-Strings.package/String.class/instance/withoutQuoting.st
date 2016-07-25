platform conventions
withoutQuoting
	"remove the initial and final quote marks, if present (and if matches nesting quotes)"
	"'''h''' withoutQuoting"
	| quote |
	self size < 2 ifTrue: [ ^self ].
	quote := self first.
	(quote = self last and: [ quote = $' or: [ quote = $" ] ])
		ifTrue: [ ^self copyFrom: 2 to: self size - 1 ]
		ifFalse: [ ^self ].