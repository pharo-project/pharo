"
Don't edit these classes. They are used else where for tests. Changing anything can make some tests fail
"
Class {
	#name : 'MockTest',
	#superclass : 'TestCase',
	#classInstVars : [
		'ignore'
	],
	#category : 'TestRunner-Tests-Utilities-Test',
	#package : 'TestRunner-Tests',
	#tag : 'Utilities-Test'
}

{ #category : 'accessing' }
MockTest class >> doNotIgnoreTest [

	ignore := false
]

{ #category : 'accessing' }
MockTest class >> ignoreTest [

	ignore := true
]

{ #category : 'class initialization' }
MockTest class >> initialize [

	self ignoreTest
]

{ #category : 'accessing' }
MockTest class >> shouldBeIgnored [

	^ ignore ifNil: [ ignore := true ]
]

{ #category : 'asserting' }
MockTest >> doNotIgnoreTest [

	self class doNotIgnoreTest
]

{ #category : 'asserting' }
MockTest >> ignoreTest [

	self class ignoreTest
]

{ #category : 'asserting' }
MockTest >> shouldBeIgnored [

	^ self class shouldBeIgnored
]
