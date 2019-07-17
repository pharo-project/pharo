Extension { #name : #Trait }

{ #category : #'*GT-InspectorExtensions-Core' }
Trait >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list 	
		title: 'Users';
		display: [ self traitUsers ]
]
