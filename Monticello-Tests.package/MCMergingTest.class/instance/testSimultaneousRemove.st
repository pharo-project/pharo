tests
testSimultaneousRemove
	self assertMerge: #(a1)
				with: #(a1)
				base: #(a1 b1)
				
				gives: #(a1)
				conflicts: #()