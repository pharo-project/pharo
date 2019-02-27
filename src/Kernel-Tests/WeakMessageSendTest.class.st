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
	
	| msg rcvr arg|
	
	rcvr := OrderedCollection new.
	arg := #( 42 ).
	msg := WeakMessageSend receiver: rcvr selector: #add: arguments: { 4 }.
	
	msg valueWithArguments: {}.
	
	self assertCollection: rcvr asArray equals: { 4 }.
	
	msg valueWithArguments: { #a . WeakArray with: arg }.
	
	self assertCollection: rcvr asArray equals: { 4 . #a }.
	
	msg valueWithArguments: (WeakArray with: arg).
	
	self assertCollection: rcvr asArray equals: { 4 . #a  . #(42) }.
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
	m := WeakMessageSend
		receiver: Array
		selector: #with:with:
		arguments: { 1 . 2 }.
	Smalltalk garbageCollectMost.
	self assert: (m value = { 1 . 2 }).

]
