expanding-collapsing
expandAllFromNode: aNode
		self expandAll: ((self nodeMorphOfNode: aNode) ifNil: [^self]).
		self adjustSubmorphPositions
