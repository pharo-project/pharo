tests
testSubtractiveConflictlessMerge
	self assertMerge: #(a1 b1)
				with: #()
				base: #(a1)
				
				gives: #(b1)
				conflicts: #()