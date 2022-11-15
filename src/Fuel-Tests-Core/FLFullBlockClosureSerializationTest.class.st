Class {
	#name : #FLFullBlockClosureSerializationTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-FullSerialization'
}

{ #category : #tests }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodes [
	"Raise an error when materializing a closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 42'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : #tests }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesClean [
	"Raise an error when materializing a closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 42'
		in: aClass.
	"clean blocks are independent"
	self shouldnt: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : #tests }
FLFullBlockClosureSerializationTest >> testBlockClosureRemoved [
	"Raise an error when materializing a closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]
