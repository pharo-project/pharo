"
This is the unit test for the class UndefinedObject. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #UndefinedObjectTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #'tests - class methods' }
UndefinedObjectTest >> testAllInstances [
	self assert: UndefinedObject allInstances size = 1 description: 'There should be a single instance of UndefinedObject'.
	self assert: (UndefinedObject allInstances includes: nil) description: 'nil should be an instance of UndefinedObject'.
]

{ #category : #'tests - copying' }
UndefinedObjectTest >> testDeepCopy [
	self assert: nil deepCopy isNil
]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testHaltIfNil [

	self should: [ nil haltIfNil] raise: Halt.
]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIfNil [

	self should: [ nil ifNil: [self halt]] raise: Halt.



]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIfNilIfNotNil [

	self should: [ nil ifNil: [self halt] ifNotNil: [self error] ] raise: Halt.



]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIfNotNil [

	nil ifNotNil: [
		self fail: 'should not get here' ].



]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIfNotNilDo [

	nil ifNotNil: [
		self fail: 'should not get here' ].

]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIfNotNilIfNil [

	self should: [ nil ifNotNil: [self error] ifNil: [self halt] ] raise: Halt.



]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIsEmptyOrNil [

	self assert: (nil isEmptyOrNil).
]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIsLiteral [

	self assert: (nil isLiteral).
]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testIsNil [

	self assert: (nil isNil).
]

{ #category : #'tests - class methods' }
UndefinedObjectTest >> testNew [

	self should: [ UndefinedObject new] raise: Error.
]

{ #category : #'tests - testing' }
UndefinedObjectTest >> testNotNil [

	self deny: (nil notNil).
]

{ #category : #'tests - printing' }
UndefinedObjectTest >> testPrintOn [

	| string |
	string := String streamContents: [:stream | nil printOn: stream].
	self assert: (string = 'nil').
]

{ #category : #'tests - copying' }
UndefinedObjectTest >> testShallowCopy [
	self assert: nil shallowCopy isNil
]

{ #category : #'tests - copying' }
UndefinedObjectTest >> testVeryDeepCopyWith [
	self assert: (nil veryDeepCopyWith: nil) isNil
]
