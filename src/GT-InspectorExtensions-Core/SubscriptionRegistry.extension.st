Extension { #name : #SubscriptionRegistry }

{ #category : #'*GT-InspectorExtensions-Core' }
SubscriptionRegistry >> gtInspectorSubscriptionsIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite table 
		title: 'Subscriptions';
		display: [ self subscriptions ];
		column: 'Class' evaluated: [ :each | each announcementClass ];
		column: 'Subscriber' evaluated: [ :each | each subscriber ]
]
