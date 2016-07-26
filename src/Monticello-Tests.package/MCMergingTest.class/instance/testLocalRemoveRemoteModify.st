tests
testLocalRemoveRemoteModify
	self assertMerge: #(b1)
				with: #(a1 b1)
				base: #(a2 b1)
				
				gives: #(a1 b1)
				conflicts: #((a1 removed)).

	self assertMerge: #(b1)
				with: #(a2 b1)
				base: #(a1 b1)
				
				gives: #(a2 b1)
				conflicts: #((a2 removed)).