additions
sameLiteralsAs: method
	"Compare my literals to those of method. This is needed to compare compiled methods."

	| numLits literal1 literal2 |
	(numLits := self numLiterals) ~= method numLiterals
		ifTrue: [ ^ false ].
	"The last literal requires special checking instead of using #literalEqual:"
	1 to: numLits - 1 do: [ :index | 
		literal1 := self literalAt: index.
		literal2 := method literalAt: index.
		(literal1 == literal2 or: [ literal1 literalEqual: literal2 ])
			ifFalse: [ ^ false ] ].
	"we can't do = on outer code or it will recursively call this code"
	^ true