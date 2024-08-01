Class {
	#name : 'FLFullBlockClosureSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodes [
	"Raise an error when materializing a clean closure whose method has changed bytecodes.
	Send #yourself to prevent ConstantBlockClosure optmization."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertCleanBlockOuterContextBasedOnCompilationOption: aClosure.

	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 42'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.

	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesClean [
	"Raise an error when materializing a non-clean closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ :x | x ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertCleanBlockOuterContextBasedOnCompilationOption: aClosure.
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 43'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesConstant [
	"Raise an error when materializing a constant closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertConstantBlockOuterContextBasedOnCompilationOption: aClosure.
	CompilationContext optionConstantBlockClosure ifTrue: [ 
		self assert: aClosure class equals: (Smalltalk at: #ConstantBlockClosure) ].
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 42 + 1'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesNonClean [
	"Raise an error when materializing a non-clean closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ self + 1 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self deny: aClosure isClean.
	self assert: aClosure outerContext isNotNil.
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 43'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureChangeSameBytecodesConstant [
	"Raise an error when materializing a constant closure whose method has changed bytecodes.
	In constant closure, the closure is replaced by the literal, so when
	the literal changes (bytecodes would still be identical) users should be warned."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertConstantBlockOuterContextBasedOnCompilationOption: aClosure.
	CompilationContext optionConstantBlockClosure ifTrue: [ 
		self assert: aClosure class equals: (Smalltalk at: #ConstantBlockClosure) ].
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 43'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureRemovedClean [
	"Raise an error when materializing a closure whose method was removed.
	Send #yourself to prevent ConstantBlockClosure optmization."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertCleanBlockOuterContextBasedOnCompilationOption: aClosure.
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureRemovedConstant [
	"Raise an error when materializing a closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertConstantBlockOuterContextBasedOnCompilationOption: aClosure.
	CompilationContext optionConstantBlockClosure ifTrue: [ 
		self assert: aClosure class equals: (Smalltalk at: #ConstantBlockClosure) ].
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests' }
FLFullBlockClosureSerializationTest >> testBlockClosureRemovedFull [
	"Raise an error when materializing a non-clean closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ self + 1 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self deny: aClosure isClean.
	self assert: aClosure outerContext isNotNil.
	
	self serializer fullySerializeMethod: aClosure compiledBlock method.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]
