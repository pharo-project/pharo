as yet unclassified
buildSubtreeFor: aNodeObject level: aLevel
	| node childs |
	
	node := AthensSimpleTreeNode new.
	node 
		subject: aNodeObject;
		extent: (nodeExtentBlock value: aNodeObject).
		
	childs := (nodeChildsBlock value: aNodeObject) collect:  [ :each | self buildSubtreeFor: each level: aLevel+1 ].
	node children: childs.

	(self rowAt: aLevel) add: node.
	
	^ node	
