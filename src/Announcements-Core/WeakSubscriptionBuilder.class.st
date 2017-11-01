"
I am a wrapper around an Announcer, used to create weak subscriptions at subscription time.

Use me like this:

anAnnouncer weak subscribe: Announcement send: #foo to: barObject.

I raise an error for block subscriptions, as they require non-existing Ephemeron support to function correctly.
"
Class {
	#name : #WeakSubscriptionBuilder,
	#superclass : #Object,
	#instVars : [
		'announcer'
	],
	#category : 'Announcements-Core'
}

{ #category : #'instance creation' }
WeakSubscriptionBuilder class >> on: announcer [
	^ self new announcer: announcer
]

{ #category : #initialize-release }
WeakSubscriptionBuilder >> announcer: anAnnouncer [
	announcer := anAnnouncer 
]

{ #category : #private }
WeakSubscriptionBuilder >> subscribe: anAnnouncementClass do: aValuable [ 
	"Do not use this message on weak announcer because it does not work. The block will hold strongly the receiver and more.
	 We need ephemerons for that'"
	"aValuable isBlock ifTrue: [  
		self error: 'Do not use this message on weak and block because it does not work. We need ephemerons for that']."
	^	announcer basicSubscribe: (
			LegacyWeakSubscription new 
				announcer: announcer;
				announcementClass: anAnnouncementClass;
				valuable: aValuable)
]

{ #category : #'wrapped protocol' }
WeakSubscriptionBuilder >> subscribe: anAnnouncementClass send: aSelector to: anObject [
	^ self subscribe: anAnnouncementClass do: (WeakMessageSend receiver: anObject selector: aSelector)
]

{ #category : #'wrapped protocol' }
WeakSubscriptionBuilder >> weak [
	"already weak"
	^ self
]

{ #category : #'wrapped protocol' }
WeakSubscriptionBuilder >> when: anAnnouncementClass do: aValuable [
	"Do not use this message on announcer weak. We did not deprecated this method because it may break some behavior."
	self error: 'Not supported'
]

{ #category : #'wrapped protocol' }
WeakSubscriptionBuilder >> when: anAnnouncementClass send: aSelector to: anObject [
	^ self subscribe: anAnnouncementClass send: aSelector to: anObject
]
