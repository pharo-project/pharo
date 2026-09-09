"
I am an abstract class to define the common behavior of strong and weak subscribsions.
My subsclasses are a single entry in a SubscriptionRegistry. Several subscriptions by the same object is possible.

I know how to make myself weak or strong, only use this capability if it can't be determined at subscribe time though, as it uses become: (for thread-safety), which is quite slow.
"
Class {
	#name : 'AbstractAnnouncementSubscription',
	#superclass : 'Object',
	#instVars : [
		'announcer',
		'announcementClass',
		'action'
	],
	#classVars : [
		'ShouldForkDelivery'
	],
	#category : 'Announcements-Core-Subscription',
	#package : 'Announcements-Core',
	#tag : 'Subscription'
}

{ #category : 'class initialization' }
AbstractAnnouncementSubscription class >> initialize [

	self shouldForkDelivery: true
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription class >> shouldForkDelivery [

	^ ShouldForkDelivery
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription class >> shouldForkDelivery: anObject [

	ShouldForkDelivery := anObject
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> action [

	^ action
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> action: anObject [

	action := anObject
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> announcementClass [

	^ announcementClass
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> announcementClass: anObject [

	announcementClass := anObject
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> announcer [

	^ announcer
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> announcer: anAnnouncer [

	announcer := anAnnouncer
]

{ #category : 'announcing' }
AbstractAnnouncementSubscription >> deliver: anAnnouncement [
	"Deliver an announcement to receiver. In case of failure, it will be handled in separate process"

	(self handlesAnnouncement: anAnnouncement) ifTrue: [
		self class shouldForkDelivery 
			ifTrue: [ [ action cull: anAnnouncement cull: announcer ]
							on: UnhandledError
							fork: [ :ex | ex pass ] ]
			ifFalse: [ action cull: anAnnouncement cull: announcer ] ]
]

{ #category : 'error handling' }
AbstractAnnouncementSubscription >> handlesAnnouncement: anAnnouncement [

	^ announcementClass handlesAnnouncement: anAnnouncement
]

{ #category : 'converting' }
AbstractAnnouncementSubscription >> makeStrong [

	^ self subclassResponsibility
]

{ #category : 'converting' }
AbstractAnnouncementSubscription >> makeWeak [

	^ self subclassResponsibility
]

{ #category : 'printing' }
AbstractAnnouncementSubscription >> printOn: aStream [

	super printOn: aStream.

	aStream
		nextPutAll: ' (';
		print: self subscriber;
		nextPutAll: ' subscribes to ';
		print: self announcementClass;
		nextPut: $)
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> subscriber [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> subscriber: aSubscriber [

	^ self subclassResponsibility
]

{ #category : 'finalization' }
AbstractAnnouncementSubscription >> unregister [

	"Nothing, for compatibility with weak subscriptions"
]

{ #category : 'accessing' }
AbstractAnnouncementSubscription >> valuable: aValuable [
	"Used when subscriber should be extracted from valuable object"

	self action: aValuable.
	self subscriber: aValuable receiver
]
