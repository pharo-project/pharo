visiting rb nodes
visitNode: aNode 
	aNode comments do: [ :comment|
		self 
			addStyle: #comment
			from: comment start
			to: comment stop ].

	^aNode acceptVisitor: self