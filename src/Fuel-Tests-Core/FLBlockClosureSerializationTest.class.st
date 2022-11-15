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
	"Raise an error when materializing a non-clean closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 42'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeSameBytecodes [
	"Tolerate materializing a non-clean closure whose method has changed but not the bytecodes."

	| aClass aClosure materializedClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 41 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ [ 42 yourself]'
		in: aClass.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.
	self assert: materializedClosure value equals: 42
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureMaterializesClassVariablesCorrectly [
	| class closure method |
	class := self classFactory silentlyNewClassWithClassVariableNames: 'ClassVariableForTesting'.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ ClassVariableForTesting ]'
		in: class.
	
	method := class methodNamed: #methodWithClosure.
	"Make sure we don't use global clusters here, which would simply perform a lookup"
	self serializer fullySerializeBehavior: method methodClass.
	closure := method
		valueWithReceiver: class new
		arguments: #().
	
	self serialize: closure.
	
	self
		assert: (self materialized compiledBlock literalAt: 1)
		equals: (class bindingOf: 'ClassVariableForTesting').
	self
		deny: (self materialized compiledBlock literalAt: 1)
		identicalTo: (class bindingOf: 'ClassVariableForTesting')
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureRemoved [
	"Raise an error when materializing a closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ [ 42 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodNotFound
]

{ #category : #'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureRemovedClean [
	"Raise no error when materializing a clean closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self shouldnt: [ self materialized ] raise: FLMethodNotFound
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

{ #category : #'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithGlobalVariable [
	| closure materializedClosure |
	closure := [ Smalltalk ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value equals: Smalltalk
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
