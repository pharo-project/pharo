"
SUnit tests for WeakMessageSend
"
Class {
	#name : #WeakMessageSendTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #tests }
WeakMessageSendTest >> testCollectArguments [

	"Fogbugz issue 18127"
	"Test is passed if no primitve fails"
	
	| msg rcvr c|
	
	rcvr := OrderedCollection new.
	c := Color green.
	msg := WeakMessageSend receiver: rcvr selector: #add: arguments: { Color black }.
	
	msg valueWithArguments: {}.
	
	self assertCollection: rcvr asArray equals: { Color black }.
	
	msg valueWithArguments: { #a . WeakArray with: c }.
	
	self assertCollection: rcvr asArray equals: { Color black . #a }.
	
	msg valueWithArguments: (WeakArray with: c).
	
	self assertCollection: rcvr asArray equals: { Color black . #a  . Color green }.
]

{ #category : #tests }
WeakMessageSendTest >> testNoArguments [

	| m |
	m := WeakMessageSend
			receiver: true
			selector: #yourself.
	self assert: (m value).

]

{ #category : #tests }
WeakMessageSendTest >> testOneArgument [

	| m |
	m := WeakMessageSend
		receiver: Array
		selector: #with:
		argument: 1.
	Smalltalk garbageCollectMost.
	self assert: m value equals: { 1 }

]

{ #category : #tests }
WeakMessageSendTest >> testOneArgumentWithGC [

	| m |
	m := WeakMessageSend
		receiver: Array
		selector: #with:
		arguments: { Object new }.
	Smalltalk garbageCollectMost.
	self assert: (m value isNil)
]

{ #category : #tests }
WeakMessageSendTest >> testReceiverWithGC [

	| m |
	m := WeakMessageSend
		receiver: Object new
		selector: #isNil.
	Smalltalk garbageCollectMost.
	self assert: (m value isNil).
]

{ #category : #tests }
WeakMessageSendTest >> testTwoArguments [
	| m |
	m := WeakMessageSend receiver: Array selector: #with:with: arguments: {1 . 2}.
	Smalltalk garbageCollectMost.
	self assert: m value equals: {1 . 2}
]
