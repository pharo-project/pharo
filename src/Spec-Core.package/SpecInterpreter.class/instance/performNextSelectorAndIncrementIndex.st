interpreting-private
performNextSelectorAndIncrementIndex
	| args numArgs selector |

	selector := (arrayToInterpret at: index).
	selector isArray not
		ifTrue: [ 
			selector := selector asSymbol.
			numArgs := selector numArgs.
			args := arrayToInterpret copyFrom: index + 1 to: index + numArgs ]
		ifFalse: [ 
			"Here I assume that if it's not a symbol, it's a collection"
			| array |
			array := selector.
			selector := array first.
			numArgs := 0.
			args := array allButFirst ].
	
	args := args collect: [ :each | self class private_interpretASpec: each model: model selector: spec selector ].

	index := index + numArgs + 1.
	^ self actionToPerformWithSelector: selector arguments: args