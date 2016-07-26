tests
testIdenticalModification
	self
		assertMerge: #(a2 b1)
				with: #(a2 b1)
				base: #(a1 b1)
				
				gives: #(a2 b1)
				conflicts: #()