accessing
relationGraphOnReverse: anObject
	relationGraph := anObject.
	self relationGraph build.
	self buildReverseRoots