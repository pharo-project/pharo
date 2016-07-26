private
addReference: aReference
	super addReference: aReference.
	self model addVersion: ((self referenceFor: aReference)
		 resolveWith: self gofer) version