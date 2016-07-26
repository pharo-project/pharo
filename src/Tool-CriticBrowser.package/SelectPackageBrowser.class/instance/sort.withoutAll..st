private
sort: aCollection withoutAll: anotherCollection
	^ self sort: 
		(aCollection asOrderedCollection 
			removeAll: (anotherCollection);
			yourself).