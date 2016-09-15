private-refreshing
buildIndexesByNode
	indexesByNode := IdentityDictionary new.
	nodes withIndexDo: [ :aNode :index |
		indexesByNode 
			at: aNode 
			put: index ]