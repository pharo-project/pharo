method accessing
removeMethod: aSelector 
	self newMethods removeKey: aSelector ifAbsent: [].
	model removeMethod: aSelector from: self.
	self removedMethods add: aSelector