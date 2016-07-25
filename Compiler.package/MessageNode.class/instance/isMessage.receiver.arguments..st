testing
isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"Answer whether selector is selSymbol, and the predicates rcvrPred and argsPred
	 evaluate to true with respect to receiver and the list of arguments.  If selSymbol or
	 either predicate is nil, it means 'don't care'.  Note that argsPred takes numArgs
	 arguments.  All block arguments are ParseNodes."

	^(selSymbol isNil or: [selSymbol==selector key]) and:
		[(rcvrPred isNil or: [rcvrPred value: receiver]) and:
			[(argsPred isNil or: [argsPred valueWithArguments: arguments])]]