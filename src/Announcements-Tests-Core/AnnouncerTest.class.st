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
	#category : #'Announcements-Tests-Core-Tests'
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

{ #category : #tests }
AnnouncerTest >> testAnnouncingReentrant [
	" Test that it is safe to announce when handling announcement,
	so announcer are reentrant "

	" self run: #testAnnouncingReentrant "

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

{ #category : #tests }
AnnouncerTest >> testNoArgBlock [
	"we are supposed to accept zero-argument blocks as actions "

	| counter |
	counter := nil.
	announcer subscribe: AnnouncementMockA do: [ counter := 1 ].
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
	announcer
		subscribe: AnnouncementMockA
		do: [ :ann | announcement := ann ].
		
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
		subscribe: AnnouncementMockB - AnnouncementMockC
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
		subscribe: (AnnouncementMockB where: [:ann | ann parameter = #expectedParam] )
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
	instance := announcer announce: (AnnouncementMockB with: #expectedParam).
	self assert: announcement equals: instance.
	
	announcement := nil.
	instance := announcer announce: (AnnouncementMockC with: #expectedParam).
	self assert: announcement equals: instance.
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSend [
	| announcement instance |
	announcer when: AnnouncementMockA send: #value: to: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement = instance.
	
	announcement := nil.
	instance := announcer announce: AnnouncementMockB new.
	self assert: announcement isNil
]

{ #category : #'tests - subscribing' }
AnnouncerTest >> testSubscribeSet [
	| announcement instance |
	announcer
		subscribe: AnnouncementMockA , AnnouncementMockC
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
		subscribe: AnnouncementMockA , AnnouncementMockB - AnnouncementMockC
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
		subscribe: AnnouncementMockA , AnnouncementMockB - AnnouncementMockC - AnnouncementMockA
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
		subscribe: AnnouncementMockA , AnnouncementMockC - AnnouncementMockA
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
	announcer
		subscribe: AnnouncementMockB
		do: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement isNil.

	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: instance.
	
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement equals: instance.
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
	announcer subscribe: AnnouncementMockA do: [ :ann :announcer2 | flag := announcer2 == announcer ].
	announcer announce: AnnouncementMockA new.
	self assert: flag
]

{ #category : #'tests - unsubscribing' }
AnnouncerTest >> testUnsubscribeBlock [
	| announcement |
	announcer
		subscribe: AnnouncementMockA
		do: [ :ann | announcement := ann ].
	announcer
		unsubscribe: self.

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
		subscribe: AnnouncementMockA , AnnouncementMockB
		do: [ :ann | announcement := ann ].
	announcer
		unsubscribe: self.

	announcement := nil.
	announcer announce: AnnouncementMockA new.
	self assert: announcement isNil.
	
	announcement := nil.
	announcer announce: AnnouncementMockB new.
	self assert: announcement isNil.
]
