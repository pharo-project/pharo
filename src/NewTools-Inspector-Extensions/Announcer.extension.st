Extension { #name : #Announcer }

{ #category : #'*NewTools-Inspector-Extensions' }
Announcer >> hasSubscriptions [
	
	^ self numberOfSubscriptions isZero not
]

{ #category : #'*NewTools-Inspector-Extensions' }
Announcer >> inspectionSubscriptions [
	<inspectorPresentationOrder: 0 title: 'Subscriptions'>
	| table |
	
	^ (table := SpTablePresenter new)
		items: registry subscriptions asOrderedCollection;
		beMultipleSelection;
		addColumn: (SpStringTableColumn 
			title: 'Subscriber' 
			evaluated: [ :each | each subscriber asString ]);
		addColumn: (SpStringTableColumn 
			title: 'Announcement' 
			evaluated: [ :each | each announcementClass printString ]);
		addColumn: (SpStringTableColumn 
			title: 'Kind' 
			evaluated: [ :each | each className ]);
		"addAction: (GLMGenericAction new
				action: [ :table | self subscriptions reset. table update ]; 
				iconName: #glamorousCancel;
				title: 'Reset All(!)' translated;
				shouldShowTitle: true);"
		contextMenu: (SpMenuPresenter new 
			addItem: [ :item | item
				name: 'Reset subscription(s)';
				action: [ 
					table selectedItems do: [ :each | self removeSubscription: each ].
					table refresh ] ];
			yourself);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
Announcer >> inspectionSubscriptionsContext: aContext [ 
	
	aContext active: self hasSubscriptions
]
