tests
testMultipleConflicts
	self assertMerge: #(a1 b3 c1)
				with: #(a1 b2 d1)
				base: #(a1 b1 c2)
				
				gives: #(a1 b3 d1)
				conflicts: #((removed c1) (b2 b3))
