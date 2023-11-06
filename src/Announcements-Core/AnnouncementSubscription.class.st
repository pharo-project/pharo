"
The subscription is a single entry in a SubscriptionRegistry.
Several subscriptions by the same object is possible.

I know how to make myself weak or strong, only use this capability if it can't be determined at subscribe time though, as it uses become: (for thread-safety), which is quite slow.
"
Class {
	#name : 'AnnouncementSubscription',
	#superclass : 'AbstractAnnouncementSubscription',
	#instVars : [
		'subscriber'
	],
	#category : 'Announcements-Core-Subscription',
	#package : 'Announcements-Core',
	#tag : 'Subscription'
}

{ #category : 'converting' }
AnnouncementSubscription >> makeStrong [
	"I am already strong. Do nothing "
]

{ #category : 'converting' }
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

{ #category : 'accessing' }
AnnouncementSubscription >> subscriber [

	^ subscriber
]

{ #category : 'accessing' }
AnnouncementSubscription >> subscriber: aSubscriber [

	subscriber := aSubscriber
]

{ #category : 'finalization' }
AnnouncementSubscription >> unregister [

	"Nothing, for compatibility with weak subscriptions"
]
