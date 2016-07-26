tests
testPreorderSelect
	| entries |
	entries := SelectVisitor preorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)