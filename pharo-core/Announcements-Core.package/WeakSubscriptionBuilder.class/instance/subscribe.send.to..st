wrapped protocol
subscribe: anAnnouncementClass send: aSelector to: anObject
	^ self subscribe: anAnnouncementClass do: (WeakMessageSend receiver: anObject selector: aSelector)