Extension { #name : #Slot }

{ #category : #'*GT-InspectorExtensions-Core' }
Slot class >> gtInspectorAllSlotsIn: composite [
 	"This provides a list of all defined Slots"
 	<gtInspectorPresentationOrder: 20>
 	composite list 
 		title: 'All Defined Slots';
 		display: [ self allSubclasses sorted: #name ascending ];
 		tags: [ :each | { each package name } ];
 		when: [self == Slot  ].
]

{ #category : #'*GT-InspectorExtensions-Core' }
Slot class >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list 
		title: 'Slot Users';
		display: [ self slotUsers sorted: #name ascending  ];
		when: [  self slotUsers notEmpty ];
		tags: [ :each | { each package name } ].
]
