private
sort: aCollection withAll: anotherCollection
	^ self sort: 
		(aCollection asOrderedCollection 
			addAll: (anotherCollection);
			yourself).