"
The subscription is a single entry in a SubscriptionRegistry.
Several subscriptions by the same object is possible.

I know how to make myself weak or strong, only use this capability if it can't be determined at subscribe time though, as it uses become: (for thread-safety), which is quite slow.
"
Class {
	#name : #AnnouncementSubscription,
	#superclass : #Object,
	#instVars : [
		'announcer',
		'announcementClass',
		'subscriber',
		'action'
	],
	#category : #'Announcements-Core-Subscription'
}

{ #category : #accessing }
AnnouncementSubscription >> action [

	^ action
]

{ #category : #accessing }
AnnouncementSubscription >> action: anObject [

	action := anObject
]

{ #category : #accessing }
AnnouncementSubscription >> announcementClass [

	^ announcementClass
]

{ #category : #accessing }
AnnouncementSubscription >> announcementClass: anObject [

	announcementClass := anObject
]

{ #category : #accessing }
AnnouncementSubscription >> announcer [

	^ announcer
]

{ #category : #accessing }
AnnouncementSubscription >> announcer: anAnnouncer [
	announcer := anAnnouncer
]

{ #category : #announcing }
AnnouncementSubscription >> deliver: anAnnouncement [
	" deliver an announcement to receiver. In case of failure, it will be handled in separate process"

	^ (self handlesAnnouncement: anAnnouncement ) ifTrue: [
		[action cull: anAnnouncement cull: announcer]
			on: UnhandledError fork: [:ex | ex pass ]]
]

{ #category : #testing }
AnnouncementSubscription >> handlesAnnouncement: anAnnouncement [

	^ announcementClass handlesAnnouncement: anAnnouncement
]

{ #category : #converting }
AnnouncementSubscription >> makeStrong [
	" i am already strong. Do nothing "
]

{ #category : #converting }
AnnouncementSubscription >> makeWeak [

	action isBlock ifTrue: [
		self error: 'Not currently available due to missing ephemerons support'
		].

	^ announcer replace: self with:
		(WeakAnnouncementSubscription new
			announcer: announcer;
			action: action asWeakMessageSend;
			subscriber: subscriber;
			announcementClass: announcementClass)
]

{ #category : #accessing }
AnnouncementSubscription >> subscriber [
	^ subscriber
]

{ #category : #accessing }
AnnouncementSubscription >> subscriber: aSubscriber [
	subscriber := aSubscriber
]

{ #category : #accessing }
AnnouncementSubscription >> valuable: aValuable [
	"Used when subscriber should be extracted from valuable object"
	self action:  aValuable.
	self subscriber: aValuable receiver
]
