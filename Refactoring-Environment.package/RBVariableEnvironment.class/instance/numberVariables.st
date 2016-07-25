accessing
numberVariables
	^self accessorMethods inject: 0
		into: [:sum :each | sum + ((self perform: each) inject: 0 into: [:s :e | s + e size])]