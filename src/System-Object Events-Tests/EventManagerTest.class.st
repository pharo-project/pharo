Class {
	#name : #EventManagerTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'eventSource',
		'eventListener',
		'succeeded'
	],
	#category : #'System-Object Events-Tests'
}

{ #category : #private }
EventManagerTest >> addArg1: arg1
addArg2: arg2 [

	eventListener
		add: arg1;
		add: arg2
]

{ #category : #private }
EventManagerTest >> getFalse [

	^false
]

{ #category : #private }
EventManagerTest >> getFalse: anArg [

	^false
]

{ #category : #private }
EventManagerTest >> getTrue [

	^true
]

{ #category : #private }
EventManagerTest >> getTrue: anArg [

	^true
]

{ #category : #private }
EventManagerTest >> heardEvent [

	succeeded := true
]

{ #category : #running }
EventManagerTest >> setUp [

	super setUp.
	eventSource := EventManager new.
	eventListener := Bag new.
	succeeded := false
]

{ #category : #running }
EventManagerTest >> tearDown [

	eventSource releaseActionMap.
	eventSource := nil.
	eventListener := nil.
	super tearDown.

]

{ #category : #'running-dependent action' }
EventManagerTest >> testBlockReceiverNoArgs [
	eventSource when: #anEvent evaluate:[self heardEvent].
	eventSource triggerEvent: #anEvent.
	self should: [succeeded]
]

{ #category : #'running-dependent action' }
EventManagerTest >> testBlockReceiverOneArg [
	eventSource when: #anEvent: evaluate:[:arg1| eventListener add: arg1].
	eventSource triggerEvent: #anEvent: with: 9.
	self should: [eventListener includes: 9]
]

{ #category : #'running-dependent action' }
EventManagerTest >> testBlockReceiverTwoArgs [
	eventSource when: #anEvent:info: evaluate:[:arg1 :arg2| self addArg1: arg1 addArg2: arg2].
	eventSource triggerEvent: #anEvent:info: withArguments: #( 9 42 ).
	self should: [(eventListener includes: 9) and: [eventListener includes: 42]]
]

{ #category : #'running-copying' }
EventManagerTest >> testCopy [
	"Ensure that the actionMap is zapped when
	you make a copy of anEventManager"

	eventSource when: #blah send: #yourself to: eventListener.
	self denyEmpty: eventSource actionMap keys.
	self assertEmpty: eventSource copy actionMap keys
]

{ #category : #'running-broadcast query' }
EventManagerTest >> testMultipleValueSuppliers [

	eventSource
		when: #needsValue
		send: #getFalse
		to: self.
	eventSource
		when: #needsValue
		send: #getTrue
		to: self.
	succeeded := eventSource triggerEvent: #needsValue.
	self should: [succeeded]
]

{ #category : #'running-broadcast query' }
EventManagerTest >> testMultipleValueSuppliersEventHasArguments [

	eventSource
		when: #needsValue:
		send: #getFalse:
		to: self.
	eventSource
		when: #needsValue:
		send: #getTrue:
		to: self.
	succeeded := eventSource triggerEvent: #needsValue: with: 'kolme'.
	self should: [succeeded]
]

{ #category : #'running-dependent action' }
EventManagerTest >> testNoArgumentEvent [

	eventSource when: #anEvent send: #heardEvent to: self.
	eventSource triggerEvent: #anEvent.
	self should: [succeeded]
]

{ #category : #'running-dependent action supplied arguments' }
EventManagerTest >> testNoArgumentEventDependentSuppliedArgument [

	eventSource when: #anEvent send: #add: to: eventListener with: 'boundValue'.
	eventSource triggerEvent: #anEvent.
	self should: [eventListener includes: 'boundValue']
]

{ #category : #'running-dependent action supplied arguments' }
EventManagerTest >> testNoArgumentEventDependentSuppliedArguments [

	eventSource 
		when: #anEvent 
		send: #addArg1:addArg2: 
		to: self 
		withArguments: #('hello' 'world').
	eventSource triggerEvent: #anEvent.
	self should: [(eventListener includes: 'hello') and: [eventListener includes: 'world']]
]

{ #category : #'running-dependent action' }
EventManagerTest >> testOneArgumentEvent [

	eventSource when: #anEvent: send: #add: to: eventListener.
	eventSource triggerEvent: #anEvent: with: 9.
	self should: [eventListener includes: 9]
]

{ #category : #'running-remove actions' }
EventManagerTest >> testRemoveActionsForEvent [

	eventSource
		when: #anEvent send: #size to: eventListener;
		when: #anEvent send: #getTrue to: self;
		when: #anEvent: send: #fizzbin to: self.
	eventSource removeActionsForEvent: #anEvent.
	self shouldnt: [eventSource hasActionForEvent: #anEvent]
]

{ #category : #'running-remove actions' }
EventManagerTest >> testRemoveActionsTwiceForEvent [

	eventSource
		when: #anEvent send: #size to: eventListener;
		when: #anEvent send: #getTrue to: self;
		when: #anEvent: send: #fizzbin to: self.
	eventSource removeActionsForEvent: #anEvent.
	self assert: (eventSource hasActionForEvent: #anEvent) not.
	eventSource removeActionsForEvent: #anEvent.
	self assert: (eventSource hasActionForEvent: #anEvent) not.
]

{ #category : #'running-remove actions' }
EventManagerTest >> testRemoveActionsWithReceiver [

	| action |
	eventSource
		when: #anEvent send: #size to: eventListener;
		when: #anEvent send: #getTrue to: self;
		when: #anEvent: send: #fizzbin to: self.
	self assert: (eventSource hasActionsWithReceiver: self).
	eventSource removeActionsWithReceiver: self.
	action := eventSource actionForEvent: #anEvent.
	self assert: (action respondsTo: #receiver).
	self assert: ((action receiver == self) not).
	self assert: ((eventSource hasActionsWithReceiver: self) not)
]

{ #category : #'running-dependent value' }
EventManagerTest >> testReturnValueWithManyListeners [

	| value newListener |
	newListener := 'busybody'.
	eventSource
		when: #needsValue
		send: #yourself
		to: eventListener.
	eventSource
		when: #needsValue
		send: #yourself
		to: newListener.
	value := eventSource triggerEvent: #needsValue.
	self should: [value == newListener]
]

{ #category : #'running-dependent value' }
EventManagerTest >> testReturnValueWithNoListeners [

	| value |
	value := eventSource triggerEvent: #needsValue.
	self should: [value == nil]
]

{ #category : #'running-dependent value' }
EventManagerTest >> testReturnValueWithOneListener [

	| value |
	eventSource
		when: #needsValue
		send: #yourself
		to: eventListener.
	value := eventSource triggerEvent: #needsValue.
	self should: [value == eventListener]
]

{ #category : #'running-broadcast query' }
EventManagerTest >> testSingleValueSupplier [

	eventSource
		when: #needsValue
		send: #getTrue
		to: self.
	succeeded := eventSource triggerEvent: #needsValue.
	self should: [succeeded]
]

{ #category : #'running-dependent action' }
EventManagerTest >> testTwoArgumentEvent [

	eventSource when: #anEvent:info: send: #addArg1:addArg2: to: self.
	eventSource triggerEvent: #anEvent:info: withArguments: #( 9 42 ).
	self should: [(eventListener includes: 9) and: [eventListener includes: 42]]
]
