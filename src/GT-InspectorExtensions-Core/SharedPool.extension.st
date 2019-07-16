Extension { #name : #SharedPool }

{ #category : #'*GT-InspectorExtensions-Core' }
SharedPool class >> gtInspectorAllSharedPoolsIn: composite [
	"This provides a list of all defined Shared Pools"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'Shared Pools';
		display: [ self subclasses sorted: #name ascending ];
		tags: [ :each | { each package name } ];
		when: [self == SharedPool  ].
]
