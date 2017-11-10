Extension { #name : #ClassDescription }

{ #category : #'*GT-InspectorExtensions-Core' }
ClassDescription >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list 	
		title: 'Users';
		when: [ self isTrait ];
		display: [ 
			self traitUsers asArray
			"Smalltalk allClasses select: [ :class |
			   class traitComposition traits includes: self ]" ]
]
