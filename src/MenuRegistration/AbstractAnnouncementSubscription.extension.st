Extension { #name : 'AbstractAnnouncementSubscription' }

{ #category : '*MenuRegistration' }
AbstractAnnouncementSubscription class >> menuCommandOn: aBuilder [

	<worldMenu>
	(aBuilder item: #'Toggle forked announcement delivery (epxert)')
		parent: #Debug;
		order: 10;
		help:
			'By default announcements are delivered in a fork in order to let the delivery proceed if an error happens in one of the subscribers. In a few cases, it might be easier to debug some actions if we disable this fork since the errors will open a debugger with a full stack instead of having a stack cut at the moment of the fork.';
		action: [ self shouldForkDelivery: self shouldForkDelivery not ]
]
