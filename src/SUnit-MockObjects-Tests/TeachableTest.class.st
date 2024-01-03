"
TestCases for the class Teachable
"
Class {
	#name : 'TeachableTest',
	#superclass : 'TestCase',
	#instVars : [
		'teachable'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'defaults' }
TeachableTest >> defaultTestClass [
	"Returns the receivers default test class"

	^Teachable
]

{ #category : 'running' }
TeachableTest >> setUp [
	"Set up the receiver for running a new test"

	super setUp.
	self teachable: self defaultTestClass new
]

{ #category : 'accessing' }
TeachableTest >> teachable [

	^teachable
]

{ #category : 'accessing' }
TeachableTest >> teachable: anObject [

	teachable := anObject
]

{ #category : 'running' }
TeachableTest >> tearDown [
	"Tear down the receiver after running a test"

	self teachable: nil.
	super tearDown
]

{ #category : 'testing' }
TeachableTest >> testAcceptSendWithBinaryMessage [
	"Test if the teachable object is able to accept a message send it is taught"

	self should: [ self teachable + 3 ] raise: MessageNotUnderstood.
	self teachable acceptSend: #+.
	self teachable + 3
]

{ #category : 'testing' }
TeachableTest >> testAcceptSendWithKeyWordMessage [
	"Test if the teachable object is able to accept a message send it is taught"

	self should: [ self teachable foo: 3 ] raise: MessageNotUnderstood.
	self teachable acceptSend: #foo:.
	self teachable foo: 3
]

{ #category : 'testing' }
TeachableTest >> testAcceptSendWithUnaryMessage [
	"Test if the teachable object is able to accept a message send it is taught"

	self should: [ self teachable foo ] raise: MessageNotUnderstood.
	self teachable acceptSend: #foo.
	self teachable foo
]

{ #category : 'testing' }
TeachableTest >> testInstanceCreation [
	"Test the instance creation of the teachable"

	| instance |
	instance := self defaultTestClass new.
	self assert: instance isNotNil.
	self assert: instance class equals: self defaultTestClass
]

{ #category : 'testing' }
TeachableTest >> testWhenSendEvaluate [
	"Tests teaching the teachable to evaluate a block"

	| called |
	called := false.
	self should: [ self teachable foo ] raise: MessageNotUnderstood.
	self teachable whenSend: #foo evaluate: [  called := true. 1 ].
	self teachable foo.
	self assert: self teachable foo equals: 1.
	self assert: called
]

{ #category : 'testing' }
TeachableTest >> testWhenSendEvaluateWithOneArgumentBlock [
	"Tests teaching the teachable to evaluate a block with an argument"

	| called |
	called := false.
	self should: [ self teachable foo: #uninteresting ] raise: MessageNotUnderstood.
	self teachable whenSend: #foo: evaluate: [ :arg | called := arg. 1 ].
	self teachable foo: #uninteresting.
	self assert: (self teachable foo: true) equals: 1.
	self assert: called.
	self assert: (self teachable foo: false) equals: 1.
	self assert: called not
]

{ #category : 'testing' }
TeachableTest >> testWhenSendWithReturnValue [
	"Tests teaching the teachable to respond to a taught message with a return value"

	| object |
	object := OrderedCollection new.
	self should: [ self teachable foo ] raise: MessageNotUnderstood.
	self teachable whenSend: #foo return: object.
	self teachable foo.
	self assert: self teachable foo identicalTo: object
]
