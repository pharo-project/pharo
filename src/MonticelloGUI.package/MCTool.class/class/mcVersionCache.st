accessing
mcVersionCache
	^ MCVersionCache
		ifNil: [ MCVersionCache := LRUCache new maximumWeight: 32 ]