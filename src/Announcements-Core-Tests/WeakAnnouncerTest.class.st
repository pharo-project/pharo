"
SUnit tests for weak announcements
"
Class {
	#name : #WeakAnnouncerTest,
	#superclass : #AnnouncerTest,
	#category : #'Announcements-Core-Tests-Tests'
}

{ #category : #benchmarks }
WeakAnnouncerTest >> benchManyWeakGuys [

	| object ann time |

	object := Object new.
	ann := Announcer new.

	50000 timesRepeat: [ (ann weak when: Announcement send: #value to: object) ].

	self assert: ann numberOfSubscriptions equals: 50000.

	Smalltalk garbageCollect.

	time := [object := nil.
		Smalltalk garbageCollect.
		 ]timeToRun.

	self assert: ann numberOfSubscriptions equals: 0.
	^ time
]

{ #category : #benchmarks }
WeakAnnouncerTest >> benchWeakSubscriptionDynamic [
	"Run me to see how expensive #becomeForward: is (compared to #benchWeakSubscriptionStatic )"

	| object ann |

	object := Object new.
	ann := Announcer new.

	^[
		1000 timesRepeat: [ (ann when: Announcement send: #value to: object) makeWeak ].
	] timeToRun
]

{ #category : #benchmarks }
WeakAnnouncerTest >> benchWeakSubscriptionStatic [
	"Run me to see how cheap object creation is (compared to #benchWeakSubscriptionDynamic ) "

	| object ann |

	object := Object new.
	ann := Announcer new.

	^[
		1000 timesRepeat: [ (ann weak when: Announcement send: #value to: object)  ].
	] timeToRun
]

{ #category : #utilities }
WeakAnnouncerTest >> longTestCase [

	self timeLimit: 60 seconds
]

{ #category : #tests }
WeakAnnouncerTest >> testNoDeadWeakSubscriptions [
	self longTestCase.
	"we skip this on the ci as it impact all other ci checks, open issue https://github.com/pharo-project/pharo/issues/2471"
	self skipOnPharoCITestingEnvironment.

	5 timesRepeat: [ Smalltalk garbageCollect ].

	self assertEmpty: (WeakAnnouncementSubscription allInstances select: [ :sub | sub subscriber isNil ]).

	self assertEmpty: (WeakAnnouncementSubscription allInstances select: [ :sub | sub subscriber isNil ])
]

{ #category : #tests }
WeakAnnouncerTest >> testNoWeakBlock [
	"There is no such thing as a weak block, these things never go away."

	<expectedFailure>

	| counter |

	self longTestCase.

	counter := 0.

	(announcer when: AnnouncementMockA do: [ :ann | counter := counter + 1 ]) makeWeak.

	Smalltalk garbageCollect.
	announcer announce: AnnouncementMockA.
	self assert: counter equals: 1.

	Smalltalk garbageCollect.
	announcer announce: AnnouncementMockA.
	self assert: counter equals: 2
]

{ #category : #tests }
WeakAnnouncerTest >> testWeakDoubleAnnouncer [

	| a1 a2 o |

	self longTestCase.

	a1 := Announcer new.
	a2 := Announcer new.

	o := Object new.

	self
	assert: a1 subscriptions numberOfSubscriptions isZero;
	assert: a2 subscriptions numberOfSubscriptions isZero.

	a1 weak when: Announcement send: #abcdef to: o.
	a2 weak when: Announcement send: #abcdef to: o.

	self
	assert: a1 subscriptions numberOfSubscriptions equals: 1;
	assert: a2 subscriptions numberOfSubscriptions equals: 1.

	3 timesRepeat: [ Smalltalk garbageCollect ].

	self
	assert: a1 subscriptions numberOfSubscriptions equals: 1;
	assert: a2 subscriptions numberOfSubscriptions equals: 1.

	o := nil.

	3 timesRepeat: [ Smalltalk garbageCollect ].

	self
	assert: a1 subscriptions numberOfSubscriptions isZero;
	assert: a2 subscriptions numberOfSubscriptions isZero
]

{ #category : #tests }
WeakAnnouncerTest >> testWeakObject [
	| counter collector forwarder |

	self longTestCase.

	counter := 0.
	collector := [ counter := counter + 1 ].
	forwarder := MessageSend receiver: collector selector: #value.
	(announcer when: AnnouncementMockA send: #value to: forwarder) makeWeak.

	" shouldn't go away, we are still holding a reference to 'forwarder': "
	Smalltalk garbageCollect.
	announcer announce: AnnouncementMockA.
	self assert: counter equals: 1.

	" should go away as we let the only reference to 'forwarder' go: "
	forwarder := nil.
	Smalltalk garbageCollect.
	announcer announce: AnnouncementMockA.
	self assert: counter equals: 1
]

{ #category : #tests }
WeakAnnouncerTest >> testWeakSubscription [
	| obj subscription |
	self longTestCase.

	obj := Object new.

	subscription := (announcer when: AnnouncementMockA send: #value to: obj) makeWeak.

	self assert: obj identicalTo: subscription subscriber
]

{ #category : #tests }
WeakAnnouncerTest >> testWeakSubscriptionRelease [
	"This test shows the a problem with weak registrations not garbage collected in Spur.
	 See https://pharo.manuscript.com/f/cases/17537 for more details."
	self benchManyWeakGuys
]
