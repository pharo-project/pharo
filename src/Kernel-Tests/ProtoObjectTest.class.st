"
This is the unit test for the class ProtoObject. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #ProtoObjectTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Objects'
}

{ #category : #'tests - testing' }
ProtoObjectTest >> testFlag [
	ProtoObject new flag: #hallo
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testIfNil [
	| object block |
	object := ProtoObject new.
	object ifNil: [ self halt ].
	self assert: (object ifNil: [ nil ]) == object.	"Now the same without inlining."
	block := [ self halt ].
	object ifNil: block.
	block := [ nil ].
	self assert: (object ifNil: block) == object
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testIfNilIfNotNil [

	| object returnValue block |
	object := ProtoObject new.
	returnValue := Object new.
	self should: [ object ifNil: [ self error ] ifNotNil: [ self halt ] ] raise: Halt.
	self should: [ object ifNil: [ self error ] ifNotNil: [ :o | self halt ] ] raise: Halt.
	self assert: (object ifNil: [ false ] ifNotNil: [ :o | o == object ]).
	self assert: (object ifNil: [ nil ] ifNotNil: [ returnValue ]) == returnValue.
	self assert: (object ifNil: [ nil ] ifNotNil: [ :o | returnValue ]) == returnValue.
	"Now the same without inlining."
	block := [ self halt ].
	self should: [ object ifNil: [ self error ] ifNotNil: block ] raise: Halt.
	block := [ :o | self halt ].
	self should: [ object ifNil: [ self error ] ifNotNil: block ] raise: Halt.
	block := [ :o | o == object ].
	self assert: (object ifNil: [ false ] ifNotNil: block).
	block := [ returnValue ].
	self assert: (object ifNil: [ nil ] ifNotNil: block) = returnValue.
	block := [ :o | returnValue ].
	self assert: (object ifNil: [ nil ] ifNotNil: block) = returnValue
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testIfNotNil [

	| object returnValue block |
	object := ProtoObject new.
	returnValue := Object new.
	self should: [ object ifNotNil: [ self halt ] ] raise: Halt.
	self should: [ object ifNotNil: [ :o | self halt ] ] raise: Halt.
	self assert: (object ifNotNil: [ :o | o == object ]).
	self assert: (object ifNotNil: [ returnValue ]) == returnValue.
	self assert: (object ifNotNil: [ :o | returnValue ]) == returnValue.	
	"Now the same without inlining."
	block := [ self halt ].
	self should: [ object ifNotNil: block ] raise: Halt.
	block := [ :o | self halt ].
	self should: [ object ifNotNil: block ] raise: Halt.
	block := [ :o | o == object ].
	self assert: (object ifNotNil: block).
	block := [ returnValue ].
	self assert: (object ifNotNil: block) = returnValue.
	block := [ :o | returnValue ].
	self assert: (object ifNotNil: block) = returnValue
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testIfNotNilIfNil [

	| object returnValue block |
	object := ProtoObject new.
	returnValue := Object new.
	self should: [ object ifNotNil: [ self halt ] ifNil: [ self error ]  ] raise: Halt.
	self should: [ object ifNotNil: [ :o | self halt ] ifNil: [ self error ] ] raise: Halt.
	self assert: (object ifNotNil: [ :o | o == object ] ifNil: [ false ]).
	self assert: (object ifNotNil: [ returnValue ] ifNil: [ false ]) == returnValue.
	self assert: (object ifNotNil: [ :o | returnValue ] ifNil: [ false ]) == returnValue.
	"Now the same without inlining."
	block := [ self halt ].
	self should: [ object ifNotNil: block ifNil: [ self error ]  ] raise: Halt.
	block := [ :o | self halt ].
	self should: [ object ifNotNil: block ifNil: [ self error ] ] raise: Halt.
	block := [ :o | o == object ].
	self assert: (object ifNotNil: block ifNil: [ false ]).
	block := [ returnValue ].
	self assert: (object ifNotNil: block ifNil: [ false ]) == returnValue.
	block := [ :o | returnValue ].
	self assert: (object ifNotNil: block ifNil: [ false ]) == returnValue
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testIsNil [

	self deny: ProtoObject new isNil
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testNotTheSame [

	| object1 object2 |
	object1 := ProtoObject new.
	object2 := ProtoObject new.
	self assert: object1 ~~ object2.
	self deny: object1 ~~ object1.
]

{ #category : #'tests - testing' }
ProtoObjectTest >> testPointersTo [
	| myObject myArray |
	myObject := Object new.
   self assert: myObject pointersTo isEmpty.
	myArray := {myObject. myObject}.
	self assert: myObject pointersTo asArray equals: {myArray}.

]

{ #category : #'tests - testing' }
ProtoObjectTest >> testPointersToCycle [

	| myObject myArray myArray2 pointingObjects |
	myObject := Object new.
	myArray := {myObject . myObject}.
	myArray2 := {myObject . myArray}.

	pointingObjects := (myObject pointersTo) asArray.
	self assert: pointingObjects size equals: 2.
	self assert: (pointingObjects includesAll: {myArray . myArray2}).
	
	"PointerFinder loops in presence of cycles"
"	myArray at: 1 put: myArray.
	pointingObjects := (PointerFinder pointersTo: myObject) asArray.
	self assert: pointingObjects = {myArray}.
"
]
