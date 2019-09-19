Extension { #name : #Cursor }

{ #category : #'*GT-InspectorExtensions-Core' }
Cursor class >> gtInspectorCursorsIn: composite [ 
	<gtInspectorPresentationOrder: 10>
	composite list 
		title: 'Cursors';
		display: [self classVariables sorted: [ :a :b | a key < b key ] ];
		icon: [ :each | each value ];
		format: [ :each | each key ]
]
