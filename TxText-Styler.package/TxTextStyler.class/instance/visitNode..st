visiting
visitNode: node
	"node comments do: [ :comment |
		self 
			addStyle: #comment
			from: comment first
			to: comment last ]."

	^node acceptVisitor: self 
