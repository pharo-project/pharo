Class {
	#name : #MicBlockTest,
	#superclass : #MicElementTest,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #testing }
MicBlockTest class >> isAbstract [ 

	^ self = MicBlockTest
]

{ #category : #tests }
MicBlockTest >> testHasProperty [
	self deny: (self instance hasProperty: #foo).
	self instance propertyAt: #foo put: 'bar'.
	self assert: (self instance hasProperty: #foo).
	self deny: (self instance hasProperty: #bar)
]

{ #category : #tests }
MicBlockTest >> testProperties [
	self assert: self instance properties isNotNil.
	self instance instVarNamed: 'properties' put: nil.

	self instance propertyAt: #foo put: #bar.
	self instance instVarNamed: 'properties' put: nil.

	self instance propertyAt: #foo ifAbsent: [ nil ].
	self instance instVarNamed: 'properties' put: nil.

	self instance propertyAt: #foo ifAbsentPut: [ #bar ].
	self instance instVarNamed: 'properties' put: nil.

	self instance hasProperty: #foo.
	self instance instVarNamed: 'properties' put: nil
]

{ #category : #tests }
MicBlockTest >> testPropertyAt [
	self assert: (self instance propertyAt: #foo put: 'bar') equals: 'bar'.
	self assert: (self instance propertyAt: #foo) equals: 'bar'.
	self should: [ self instance propertyAt: #bar ] raise: MicPropertyError
]

{ #category : #tests }
MicBlockTest >> testPropertyAtIfAbsent [
	self assert: (self instance propertyAt: #foo put: 'bar') equals: 'bar'.
	self assert: (self instance propertyAt: #foo ifAbsent: [ 'baz' ]) equals: 'bar'.
	self assert: (self instance propertyAt: #bar ifAbsent: [ 'baz' ]) equals: 'baz'
]

{ #category : #tests }
MicBlockTest >> testPropertyAtPut [
	self instance propertyAt: #foo put: 'bar'.
	self assert: (self instance propertyAt: #foo) equals: 'bar'.

	self instance propertyAt: #foo put: 'baz'.
	self assert: (self instance propertyAt: #foo) equals: 'baz'
]

{ #category : #tests }
MicBlockTest >> testSanity [
	"If this test case fails, there is something wrong with the setup of the test-case."

	self
		assert: self testedClass isAbstract not
		description: 'Unable to test abstract class.'.
	self
		assert: self instance class equals: self testedClass
		
]
