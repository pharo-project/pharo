"
A WeakAnnouncementSubscription is a subscription which is removed automatically when the subscriber is unreferenced.

No support for ephemerons currently prevents this from working for Block actions (blocks hold their receiver, which is the default subscriber strongly). 

To switch between subscription types, use makeStrong/makeWeak on the subscription returned when initially registering with announcer.


Note, that list and next must be first instance variables.
"
Class {
	#name : 'WeakAnnouncementSubscription',
	#superclass : 'AbstractAnnouncementSubscription',
	#type : 'weak',
	#instVars : [
		'next'
	],
	#category : 'Announcements-Core-Subscription',
	#package : 'Announcements-Core',
	#tag : 'Subscription'
}

{ #category : 'instance creation' }
WeakAnnouncementSubscription class >> new [
	^ (self basicNew: 1) initialize
]

{ #category : 'finalization' }
WeakAnnouncementSubscription >> finalize [

	announcer removeSubscription: self
]

{ #category : 'converting' }
WeakAnnouncementSubscription >> makeStrong [

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

{ #category : 'converting' }
WeakAnnouncementSubscription >> makeWeak [

	^ self
]

{ #category : 'accessing' }
WeakAnnouncementSubscription >> next [

	^ next
]

{ #category : 'finalization' }
WeakAnnouncementSubscription >> register [

	self finalizationRegistry add: self subscriber finalizer: self
]

{ #category : 'accessing' }
WeakAnnouncementSubscription >> subscriber [

	^ self basicAt: 1
]

{ #category : 'accessing' }
WeakAnnouncementSubscription >> subscriber: anObject [

	self subscriber ifNotNil: [ self error: 'subscriber is already set' ].
	self basicAt: 1 put: anObject.
	self register
]

{ #category : 'finalization' }
WeakAnnouncementSubscription >> unregister [

	self subscriber ifNotNil: [:sub | 
		self finalizationRegistry 
			remove: sub 
			ifAbsent: ["I will ignore if the subscription has been already removed"]]
]
