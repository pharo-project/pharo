accessing
parents
	^ parent 
		ifNil: [ OrderedCollection with: self ]
		ifNotNil: [ 
			parent parents
				addLast: self;
				yourself ]