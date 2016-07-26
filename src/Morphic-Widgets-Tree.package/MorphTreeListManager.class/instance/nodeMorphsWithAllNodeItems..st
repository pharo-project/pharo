accessing
nodeMorphsWithAllNodeItems: aNodeItemList
	| result |

	result := OrderedCollection new.
	
	self allNodeMorphs do: [:m | 
		aNodeItemList do: [ :sel | (m expandPath: sel) ] ].
	
	self allNodeMorphs do: [:m | 
		aNodeItemList do: [ :sel | (m matchPath: sel)
			ifNotNil: [:col | result addAll: col ] ] ].

	^ result flattened