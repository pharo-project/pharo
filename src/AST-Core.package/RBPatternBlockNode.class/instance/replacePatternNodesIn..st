matching
replacePatternNodesIn: aRBBlockNode 
	aRBBlockNode body nodesDo: 
			[:each | 
			(each isVariable and: [each isPatternNode]) 
				ifTrue: 
					[each 
						replaceWith: (self constructLookupNodeFor: each name in: aRBBlockNode)]]