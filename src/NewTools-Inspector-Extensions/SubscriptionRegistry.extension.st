Extension { #name : #SubscriptionRegistry }

{ #category : #'*NewTools-Inspector-Extensions' }
SubscriptionRegistry >> inspectionSubscriptions [
	<inspectorPresentationOrder: 0 title: 'Subscriptions'>

	^ SpTablePresenter new
		items: self subscriptions asOrderedCollection;
		addColumn: (SpStringTableColumn title: 'Class' evaluated: [ :each | each announcementClass ]);
		addColumn: (SpStringTableColumn title: 'Subscriber' evaluated: [ :each | each subscriber ]);
		yourself
]
