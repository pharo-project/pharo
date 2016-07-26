tests
testOptimizedBlockLocalNilling1
	"Whether a block is optimized or not a block-local temp
	 should be nil at the start of each evaluation of the block."

	1 to: 3 do: [:i| | j |
		self assert: j isNil.
		j := i + 1.
		self assert: j isNil not]