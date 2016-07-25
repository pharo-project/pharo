dependencies
addDependentToHierachy: anObject
	self addDependent: anObject.
	self tests do: [ :each | each addDependentToHierachy: anObject]
			