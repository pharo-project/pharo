"
The implementation uses a threadsafe subscription registry, in the sense that registering, unregistering, and announcing from an announcer at the same time in different threads should never cause failures.


The method #prevent:during: allows one to prevent the firing of some announcements during the execution of a block wile letting the other announcements go throuht.
"
Class {
	#name : #Announcer,
	#superclass : #Object,
	#instVars : [
		'registry',
		'preventedAnnouncements'
	],
	#category : #'Announcements-Core-Base'
}

{ #category : #announce }
Announcer >> announce: anAnnouncement [

	| announcement |
	announcement := anAnnouncement asAnnouncement.

	"If the user required to prevent the current announcement, we stop here."
	(self preventedAnnouncements handlesAnnouncement: announcement) ifTrue: [ ^ self ].

	registry ifNotNil: [ registry deliver: announcement ].
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

{ #category : #announce }
Announcer >> prevent: anAnnouncementClass during: aBlock [
	"This method allows one to ensure that an Announcement class or an AnnouncementSet will never be announced during the execution of a block.
	One of the goal can be for example to allow to send messages that should announce something without doing the announcement multiple times while letting the other announcements pass throught."

	| previousValue |
	previousValue := self preventedAnnouncements copy.
	[
	preventedAnnouncements := self preventedAnnouncements , anAnnouncementClass.
	aBlock value ] ensure: [ preventedAnnouncements := previousValue ]
]

{ #category : #accessing }
Announcer >> preventedAnnouncements [

	^ preventedAnnouncements ifNil: [ preventedAnnouncements := AnnouncementSet new ]
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

{ #category : #subscription }
Announcer >> when: anAnnouncementClass do: aValuable for: aSubscriber [
	"Subscribe aSubscriber to announcements of anAnnouncementClass class. 
	 When the announcement is raised, aValuable is executed."

	^ registry add: (AnnouncementSubscription new
			   announcer: self;
			   announcementClass: anAnnouncementClass;
			   action: aValuable;
			   subscriber: aSubscriber;
			   yourself)
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
		for: anObject
]
