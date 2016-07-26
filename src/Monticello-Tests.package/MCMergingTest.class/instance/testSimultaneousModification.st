tests
testSimultaneousModification
	self assertMerge: #(a2)
				with: #(a3)
				base: #(a1)
				
				gives: #(a3)
				conflicts: #((a3 a2)).