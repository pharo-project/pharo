tests
testComplexConflictlessMerge
	self 
		assertMerge: #(a1 b1 d1)
				with: #(a2 c1)
				base: #(a1 c1 d1)
				
				gives: #(a2 b1)
				conflicts: #()