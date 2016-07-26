testing
isRequired: marker
	marker ifNil: [^ false].
	(self isImplicitlyRequired: marker) ifTrue: [^ true].
	(self isExplicitlyRequired: marker) ifTrue: [^ true]. 
	(self isSubclassResponsibility: marker) ifTrue: [^ true]. 
	^ false