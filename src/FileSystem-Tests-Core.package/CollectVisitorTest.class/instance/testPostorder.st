tests
testPostorder
	| entries |
	entries := CollectVisitor postorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha/epsilon'
				'/alpha'
			)