private
guessCascadeReceiver: aNumber 
	| type |
	aNumber 
		to: 1
		by: -1
		do: 
			[ :index | 
			type := (ranges at: index) type.
			((#(#statementSeparator #assignment #ansiAssignment ) includes: type) or: [ (type beginsWith: 'pattern') or: [ type beginsWith: 'methodTemp' ] ]) ifTrue: [ ^ self guessTypeOf: index + 1 ] ].
	^ nil