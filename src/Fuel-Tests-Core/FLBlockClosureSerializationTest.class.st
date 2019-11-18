"
SUnit tests for fuel serialization of block closures
"
Class {
	#name : #FLBlockClosureSerializationTest,
	#superclass : #FLSerializationTest,
	#classVars : [
		'ClassVariableForTesting'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #'closures for testing' }
FLBlockClosureSerializationTest class >> blockClosureWithSelfSend [
	^ [ self printString ]
]

{ #category : #'closures for testing' }
FLBlockClosureSerializationTest class >> blockClosureWithTempVariableRead [
	| string |
	string := 'test'.
	^ [ string asUppercase ].
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodes [
	"Raise an error when materializing a closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self newClass
		duringTestCompileSilently: 'methodWithClosure  ^ [ 42 ]';
		yourself.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass duringTestCompileSilently: 'methodWithClosure  ^ 42'.
	self should: [ self materialized ] raise: FLMethodChanged
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeSameBytecodes [
	"Tolerate materializing a closure whose method has changed but not the bytecodes."

	| aClass aClosure materializedClosure |
	aClass := self newClass
		duringTestCompileSilently: 'methodWithClosure  ^ [ 41 ]';
		yourself.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass duringTestCompileSilently: 'methodWithClosure  ^ [ 42 ]'.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.
	self assert: materializedClosure value equals: 42
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureRemoved [
	"Raise an error when materializing a closure whose method was removed."

	| aClass aClosure |
	aClass := self newClass
		duringTestCompileSilently: 'methodWithClosure  ^ [ 42 ]';
		yourself.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodNotFound
]

{ #category : #'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithClassVariableRead [
	| closure materializedClosure |
	ClassVariableForTesting := nil.
	closure := [ ClassVariableForTesting ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.

	ClassVariableForTesting := true.
	self assert: materializedClosure value.
	ClassVariableForTesting := false.
	self deny: materializedClosure value
]

{ #category : #'tests-unclean' }
FLBlockClosureSerializationTest >> testBlockClosureWithSelfSend [
	| closure materializedClosure |
	closure := self class blockClosureWithSelfSend.
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value equals: closure value
]

{ #category : #'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithTempVariableRead [
	| closure materializedClosure |
	closure := self class blockClosureWithTempVariableRead.
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value equals: 'TEST'
]

{ #category : #'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithThreeArguments [
	| closure materializedClosure |
	closure := [ :a :b :c | a + b + c ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: (materializedClosure value: 1 value: 2 value: 3) equals: 6
]

{ #category : #'tests-clean' }
FLBlockClosureSerializationTest >> testNestedBlockClosure [
	| closure materializedClosure |
	closure := [ [ 42 ] ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value value equals: 42
]
