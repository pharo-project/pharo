private
addReference: aReference
	super addReference: aReference.
	self model operations 
		addAll: (self patchsetOf: aReference) operations