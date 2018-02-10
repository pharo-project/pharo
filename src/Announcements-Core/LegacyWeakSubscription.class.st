"
I am a subclass which work when VM does not supports finalization lists.
I am about 3 times slower when it comes to finalizing items
"
Class {
	#name : #LegacyWeakSubscription,
	#superclass : #WeakAnnouncementSubscription,
	#type : #weak,
	#category : #'Announcements-Core-Subscription'
}

{ #category : #converting }
LegacyWeakSubscription >> makeStrong [

	| sub |
	sub := self subscriber.
	sub ifNil: [ ^ self error: 'Subscriber is nil, cannot make strong subscription' ].
	
	self unregister.

	^ self becomeForward: (AnnouncementSubscription new
		announcer: announcer;
		action: action asMessageSend;
		subscriber: sub;
		announcementClass: announcementClass)

]

{ #category : #finalization }
LegacyWeakSubscription >> register [
	self weakRegistry 
		add: self subscriber executor: self
]

{ #category : #accessing }
LegacyWeakSubscription >> subscriber: anObject [

	self subscriber ifNotNil: [ self error: 'subscriber already set' ].
	self basicAt: 1 put: anObject.
	self register
]

{ #category : #finalization }
LegacyWeakSubscription >> unregister [

	self subscriber ifNotNil: [:sub |  self weakRegistry remove: sub ]
		
]

{ #category : #finalization }
LegacyWeakSubscription >> weakRegistry [
	^ WeakRegistry default
]
