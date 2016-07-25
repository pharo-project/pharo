tests
testAdditiveConflictlessMerge
	self
		assertMerge: #(a1 b1)
				with: #(a1 c1)
				base: #(a1)
			
				gives: #(a1 b1 c1)
				conflicts: #()