*GT-InspectorExtensions-Core
gtInspectorSubscriptionsIn: composite
	<gtInspectorPresentationOrder: 30>
	composite table
		title: 'Subscriptions';
		display: [ self subscriptions glmSubscriptions ];
		when: [ self numberOfSubscriptions isZero not ];
		column: 'Subscriber' evaluated: [ :each | each subscriber asString ];
		column: 'Announcement' evaluated: [ :each | each announcementClass name ];
		column: 'Kind' evaluated: [ :each | each class name ];
		act: [ :table | 
			self subscriptions reset.
			table update ] icon: GLMUIThemeExtraIcons glamorousCancel entitled: 'Reset';
		selectionAct: [ :table | 
			self subscriptions remove: table selection.
			table update ] entitled: 'Remove subscription'