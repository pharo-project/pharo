node access
hasNodeContents: node
	hasChildrenSelector ifNil: [ ^ node contents isNotEmpty ].
	^ model perform: hasChildrenSelector with: node item