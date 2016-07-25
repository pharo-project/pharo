tests
testLocalModifyRemoteRemove
	self assertMerge: #(a2 b1)
				with: #(b1)
				base: #(a1 b1)
				
				gives: #(b1)
				conflicts: #((removed a2)).
				
	self assertMerge: #(a1 b1)
				with: #(b1)
				base: #(a2 b1)
				
				gives: #(b1)
				conflicts: #((removed a1)).