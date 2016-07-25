tests
testOptimizedBlockLocalNilling2
	"Whether a block is optimized or not a block-local temp
	 should be nil at the start of each evaluation of the block."

	1 to: 6 do: [:i| | j k |
		self assert: j isNil.
		self assert: k isNil.
		i even
			ifTrue: [j := i + 2]
			ifFalse: [k := i + 1].
		self assert: (j isNil or: [k isNil]).
		self assert: (j isNil not or: [k isNil not])]