"
Tests verifiying the behaviour of the default SystemAnnouncer instance.
"
Class {
	#name : #SystemAnnouncerLiveTest,
	#superclass : #TestCase,
	#category : #'Tests-SystemChangeNotification'
}

{ #category : #tests }
SystemAnnouncerLiveTest >> testNoDeadSubscriptions [
	| dead |
	
	"we skip this on the ci as it impact all other ci checks, open issue https://github.com/pharo-project/pharo/issues/2471"
	
	self skipOnPharoCITestingEnvironment.

	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	dead := SystemAnnouncer uniqueInstance 
		subscriptions subscriptions select: [ :sub | 
			sub subscriber isNil 
		].
	
	self assert: dead asArray equals: #()
]

{ #category : #tests }
SystemAnnouncerLiveTest >> testOnlyWeakSubscriptions [

	| strong |

	"only weak subscriptions should be allowed to be added to the SystemAnnouncer..."
	
	strong := SystemAnnouncer uniqueInstance 
		subscriptions subscriptions reject: [ :each |
			each isKindOf: WeakAnnouncementSubscription
		].
	
	self assert: strong asArray equals: #()
]

{ #category : #tests }
SystemAnnouncerLiveTest >> testSaneSubscriptionSize [
	
	"self flag: 'this should be a release test, not a regular one'"
	"self assert: SystemAnnouncer uniqueInstance subscriptions numberOfSubscriptions < 75."
]
