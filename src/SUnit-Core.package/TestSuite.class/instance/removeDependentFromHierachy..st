dependencies
removeDependentFromHierachy: anObject
	self removeDependent: anObject.
	self tests do: [ :each | each removeDependentFromHierachy: anObject]
			