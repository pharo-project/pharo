accessing
childrenValueOf: anObject atLevel: anInteger
	self flag: 'this is rather hackish, but needed to treat list and tree in the same way.'.
	^ OrderedCollection new