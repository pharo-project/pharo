Class {
	#name : #UtilityTest,
	#superclass : #TestCase,
	#classInstVars : [
		'ignore'
	],
	#category : #'TestRunner-Tests-Utilities-Test'
}

{ #category : #accessing }
UtilityTest class >> doNotIgnoreTest [

	ignore := false
]

{ #category : #accessing }
UtilityTest class >> ignoreTest [

	ignore := true
]

{ #category : #accessing }
UtilityTest class >> initialize [

	self ignoreTest
]

{ #category : #accessing }
UtilityTest class >> shouldBeIgnored [

	^ ignore ifNil: [ self ignoreTest ]
]

{ #category : #asserting }
UtilityTest >> doNotIgnoreTest [

	self class doNotIgnoreTest
]

{ #category : #asserting }
UtilityTest >> ignoreTest [

	self class ignoreTest
]

{ #category : #asserting }
UtilityTest >> shouldBeIgnored [

	^ self class shouldBeIgnored
]
