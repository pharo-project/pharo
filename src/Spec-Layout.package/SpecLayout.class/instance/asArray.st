converting
asArray
	^ (OrderedCollection new
		add: self type;
		addAll: self privateAsArray;
		yourself) asArray