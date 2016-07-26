accessing
path
	^ parent 
		ifNil: [OrderedCollection with: self]
		ifNotNil: [(parent path) add: self; yourself]