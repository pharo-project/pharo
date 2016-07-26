tests
testPostorderSelect
	| entries |
	entries := SelectVisitor postorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha'
			)