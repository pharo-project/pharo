"
An AnnouncerTest is a test class used to test Announcer.

Instance Variables
	announcer:		<Announcer>  the announcer to test

announcer
	- the announcer that is tested

"
Class {
	#name : #AnnouncerTest,
	#superclass : #TestCase,
	#instVars : [
		'announcer'
	],
	#category : #'Announcements-Core-Tests-Tests'
}

{ #category : #testing }
AnnouncerTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #private }
AnnouncerTest >> newAnnouncer [
	^ Announcer new
]

{ #category : #running }
AnnouncerTest >> setUp [
	super setUp.
	announcer := self newAnnouncer
]

{ #category : #'tests - subscribers' }
AnnouncerTest >> testAccessingSubscribers [
	| subscribers |
	subscribers := announcer subscriptionsForClass: self class.
	self assert: subscribers size equals: 0.

	"we do not use constant blocks here, instead use a clean block.

	Using Constant blocks with announcements makes no senese, as they do nothing.

   For Clean Blocks, we have to the Announcments API as they  require to access the receiver of the home context, for clean blocks this has to be fixed, see https://github.com/pharo-project/pharo/issues/11220"
	announcer when: AnnouncementMockA do: [ nil yourself ]."1"
	announcer when: AnnouncementMockB do: [ nil yourself ]."2"
	subscribers := announcer subscriptionsForClass: self class.
	self assert: subscribers size equals: 2.

	subscribers do: [ :subscriber |	announcer unsubscribe: subscriber ].

	subscribers := announcer subscriptionsForClass: self class.
	self assert: subscribers size equals: 0
]

{ #category : #tests }
AnnouncerTest >> testAnnounceClass [
	self assert: (announcer announce: AnnouncementMockA) class equals: AnnouncementMockA
]

{ #category : #tests }
AnnouncerTest >> testAnnounceInstance [
	| instance |
	instance := AnnouncementMockA new.
	self assert: (announcer announce: instance) equals: instance
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testAnnounceWorkWithinExceptionHandlerBlocks [
	|  found |

	announcer when: Announcement do: [ found := true ].

	[ NotFound signal ]
		on: NotFound
		do: [ announcer announce: Announcement new ].

	self assert: found
]

{ #category : #tests }
AnnouncerTest >> testAnnouncingReentrant [
	"Test that it is safe to announce when handling announcement,
	so announcer are reentrant"

	| bool ok |

	ok := bool := false.
	announcer when: Announcement do: [
		bool ifFalse: [
			bool := true.
		announcer announce:  Announcement new. ]
		ifTrue: [ ok := true ]
	].

	self should: [ announcer announce: Announcement new. ] notTakeMoreThan: 1 second.
	self assert: ok
]

{ #category : #'tests - subscribers' }
AnnouncerTest >> testHandleSubscriberClass [
	"we do not use constant blocks here, instead use a clean block.

	Using Constant blocks with announcements makes no senese, as they do nothing.

   For Clean Blocks, we have to the Announcments API as they  require to access the receiver of the home context, for clean blocks this has to be fixed, see https://github.com/pharo-project/pharo/issues/11220"
	announcer when: AnnouncementMockA do: [ nil yourself ].
	self assert: (announcer handleSubscriberClass: self class).
	self deny: (announcer handleSubscriberClass: AnnouncementMockA)
]

{ #category : #tests }
AnnouncerTest >> testNoArgBlock [
	"we are supposed to accept zero-argument blocks as actions "

	| counter |
	counter := nil.
	announcer when: AnnouncementMockA do: [ counter := 1 ].
	announcer announce: AnnouncementMockA new.
	self assert: counter equals: 1
]

{ #category : #tests }
AnnouncerTest >> testPreparationAnnouncementDelivery [
	| announcement |

	announcer when: AnnouncementMockC send: #value: to: [ :ann | #firstBlock ].
	announcer when: AnnouncementMockC send: #value: to: [ :ann | #lastBlock ].

	announcement := AnnouncementMockC new.
	announcer announce: announcement.
	self assert: announcement announcingCounter equals: 1.
	announcer announce: announcement.
	self assert: announcement announcingCounter equals: 2.
	announcement := announcer announce: AnnouncementMockC.
	self assert: announcement announcingCounter equals: 1
]

{ #category : #tests }
AnnouncerTest >> testPreparationAnnouncementDeliveryWhenNoSubscriptions [

	| instance |
	announcer when: AnnouncementMockA send: #value: to: [ :ann | ].

	instance := announcer announce: AnnouncementMockC.

	self assert: instance announcingCounter equals: 0
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeBlock [
	| announcement instance |
	announcer when: AnnouncementMockA do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement isNil
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeClassWithExclusion [
	| announcement instance |
	announcer
		when: AnnouncementMockB - AnnouncementMockC
		do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement isNil
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeOnSpecificAnnouncer [
	| announcement instance |
	announcer
		when: (AnnouncementMockB where: [ :ann | ann parameter = #expectedParam ])
		do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement equals: nil.
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: nil.
	announcement := nil.
	instance := announcer announce: (AnnouncementMockB with: #wrongParam).
	self assert: announcement equals: nil.
	announcement := nil.
	instance := announcer
		announce: (AnnouncementMockB with: #expectedParam).
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer
		announce: (AnnouncementMockC with: #expectedParam).
	self assert: announcement equals: instance
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSend [
	| announcement instance |
	announcer when: AnnouncementMockA send: #value: to: [ :ann | announcement := ann ].

	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement equals: instance.

	announcement := nil.
	instance := announcer announce: AnnouncementMockB new.
	self assert: announcement isNil
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSet [
	| announcement instance |
	announcer
		when: AnnouncementMockA , AnnouncementMockC
		do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement isNil.
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement equals: instance
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSetWithExclusion [
	| announcement instance |
	announcer
		when: (AnnouncementMockA , AnnouncementMockB) - AnnouncementMockC
		do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement isNil
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSetWithExclusionOfMultipleAnnouncements [
	| announcement instance |
	announcer
		when:
			(AnnouncementMockA , AnnouncementMockB) - AnnouncementMockC
				- AnnouncementMockA
		do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement isNil.
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement isNil
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSetWithExclusionOfSetItem [
	| announcement instance |
	announcer
		when: (AnnouncementMockA , AnnouncementMockC) - AnnouncementMockA
		do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement isNil.
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement equals: instance
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSubclass [
	| announcement instance |
	announcer when: AnnouncementMockB do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement isNil.
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: instance.
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement equals: instance
]

{ #category : #'tests - subscribers' }
AnnouncerTest >> testSubscribersForClass [
	| subscribers mockA mockB |
	self assert: announcer numberOfSubscriptions equals: 0.

	mockA := AnnouncerSubscriberMockA new.
	mockA announcer: announcer.
	mockB := AnnouncerSubscriberMockB new.
	mockB announcer: announcer.
	mockA registerEvents.
	mockB registerEvents.

	self assert: announcer numberOfSubscriptions > 0.
	subscribers := announcer subscriptionsForClass: AnnouncerSubscriberMockA.
	self assert: subscribers size equals: 2.
	"AnnouncerSubscriberMockB inherits AnnouncerSubscriberMockA"
	subscribers := announcer subscriptionsForClass: AnnouncerSubscriberMockB.
	self assert: subscribers size equals: 1.
	subscribers := announcer subscriptionsForClass: self class.
	self assert: subscribers size equals: 0
]

{ #category : #tests }
AnnouncerTest >> testSymbolIdentifier [
	| passed |
	passed := false.

	[announcer
		when: #FOO send: #bar to: nil;
		announce: #FOO ]
		on: MessageNotUnderstood
		do: [ :ex | passed := (ex message selector = #bar). ].

	self assert: passed
]

{ #category : #tests }
AnnouncerTest >> testTwoArgBlock [
	"we are supposed to accept two-argument blocks as actions "

	| flag |
	flag := false.
	announcer
		when: AnnouncementMockA
		do: [ :ann :announcer2 | flag := announcer2 == announcer ].
	announcer announce: AnnouncementMockA new.
	self assert: flag
]

{ #category : #'tests - unsubscribing' }
AnnouncerTest >> testUnsubscribeBlock [
	| announcement |
	announcer when: AnnouncementMockA do: [ :ann | announcement := ann ].
	announcer unsubscribe: self.
	announcement := nil.
	announcer announce: AnnouncementMockA new.
	self assert: announcement isNil
]

{ #category : #'tests - unsubscribing' }
AnnouncerTest >> testUnsubscribeSend [
	| announcement receiver |
	announcer when: AnnouncementMockA send: #value: to: (receiver := [ :ann | announcement := ann ]).
	announcer unsubscribe: receiver.

	announcement := nil.
	announcer announce: AnnouncementMockA new.
	self assert: announcement isNil
]

{ #category : #'tests - unsubscribing' }
AnnouncerTest >> testUnsubscribeSet [
	| announcement |
	announcer
		when: AnnouncementMockA , AnnouncementMockB
		do: [ :ann | announcement := ann ].
	announcer unsubscribe: self.
	announcement := nil.
	announcer announce: AnnouncementMockA new.
	self assert: announcement isNil.
	announcement := nil.
	announcer announce: AnnouncementMockB new.
	self assert: announcement isNil
]
