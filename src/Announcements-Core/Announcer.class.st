"
The implementation uses a threadsafe subscription registry, in the sense that registering, unregistering, and announcing from an announcer at the same time in different threads should never cause failures.
"
Class {
	#name : #Announcer,
	#superclass : #Object,
	#instVars : [
		'registry'
	],
	#category : #'Announcements-Core-Base'
}

{ #category : #announce }
Announcer >> announce: anAnnouncement [

	| announcement |
	announcement := anAnnouncement asAnnouncement.
	registry ifNotNil: [
		registry deliver: announcement
		].
	^ announcement
]

{ #category : #private }
Announcer >> basicSubscribe: subscription [
	^ registry add: subscription
]

{ #category : #testing }
Announcer >> handleSubscriberClass: eventClass [
	^ self subscriptions
		ifNil: [ false ]
		ifNotNil: [:subscriptions | subscriptions handleSubscriberClass: eventClass]
]

{ #category : #testing }
Announcer >> hasSubscriber: anObject [

	registry subscriptionsOf: anObject do: [:each | ^ true].
	^ false
]

{ #category : #initialization }
Announcer >> initialize [
	super initialize.
	registry := SubscriptionRegistry new
]

{ #category : #statistics }
Announcer >> numberOfSubscriptions [
	^ registry numberOfSubscriptions
]

{ #category : #subscription }
Announcer >> removeSubscription: subscription [
	"Remove the given subscription from the receiver"

	^ registry remove: subscription
]

{ #category : #private }
Announcer >> replace: subscription with: newOne [
	^ registry replace: subscription with: newOne
]

{ #category : #accessing }
Announcer >> subscriptions [

	^ registry
]

{ #category : #accessing }
Announcer >> subscriptionsForClass: subscriberClass [
	"Return the list of subscription for a given class"
	^ self subscriptions subscriptionsForClass: subscriberClass
]

{ #category : #subscription }
Announcer >> unsubscribe: anObject [
	"Unsubscribe all subscriptions of anObject from the receiver"

	registry removeSubscriber: anObject
]

{ #category : #weak }
Announcer >> weak [
	"announcer weak subscribe: foo"
	^ WeakSubscriptionBuilder on: self
]

{ #category : #'registration api' }
Announcer >> when: anAnnouncementClass do: aValuable [
	"Declare that when anAnnouncementClass is raised, aValuable is executed.  Pay attention that such method as well as #when:do: should not be used on weak announcer since the block holds the receiver and more strongly."

	^ registry add: (
		AnnouncementSubscription new
			announcer: self;
			announcementClass: anAnnouncementClass;
			valuable: aValuable)
]

{ #category : #subscription }
Announcer >> when: anAnnouncementClass do: aValuable for: aSubscriber [
	"Declare that when anAnnouncementClass is raised, aValuable is executed and define the subscriber."

    ^ (self when: anAnnouncementClass do: aValuable) subscriber: aSubscriber; yourself
]

{ #category : #'registration api' }
Announcer >> when: anAnnouncementClass send: aSelector to: anObject [
	"Declare that when anAnnouncementClass is raised, anObject should receive the message aSelector.
    When the message expects one argument (eg #fooAnnouncement:) the announcement is passed as argument.
    When the message expects two arguments (eg #fooAnnouncement:announcer:) both the announcement and
    the announcer are passed as argument"

	^ self
		when: anAnnouncementClass
		do: (MessageSend receiver: anObject selector: aSelector)
]
