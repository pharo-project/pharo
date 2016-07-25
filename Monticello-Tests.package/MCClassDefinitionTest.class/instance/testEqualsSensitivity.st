tests
testEqualsSensitivity
	| message a b defA args defB |
	message := self creationMessage.
	a := #(ClassA SuperA CategoryA #(iVarA) #(CVarA) #(PoolA) #(ciVarA)
			typeA 'A comment' 'A').
	b := #(ClassB SuperB CategoryB #(iVarB) #(CVarB) #(PoolB) #(ciVarB)
			typeB 'B comment' 'B').
	
	defA := message valueWithArguments: a.
	1 to: 8 do: [:index |
				args := a copy.
				args at: index put: (b at: index).
				defB := message valueWithArguments: args.
				self deny: defA = defB.]