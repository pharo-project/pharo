"
SUnit tests for fuel serialization of block closures
"
Class {
	#name : 'FLBlockClosureSerializationTest',
	#superclass : 'FLSerializationTest',
	#classVars : [
		'ClassVariableForTesting'
	],
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'closures for testing' }
FLBlockClosureSerializationTest class >> blockClosureWithSelfSend [
	^ [ self printString ]
]

{ #category : 'closures for testing' }
FLBlockClosureSerializationTest class >> blockClosureWithTempVariableRead [
	| string |
	string := 'test'.
	^ [ string asUppercase ].
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesClean [
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
	 
	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ 42'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesConstant [
	"Tolerate materializing a clean closure whose method has changed but not the bytecodes."

	| aClass aClosure materializedClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 41 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assert: aClosure outerContext isNil.
	self assert: aClosure class equals: (Smalltalk at: #ConstantBlockClosure).
	
	self serialize: aClosure.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ 42 + 1'
		in: aClass.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.

	self assert: materializedClosure value equals: 41
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeDifferentBytecodesFull [
	"Raise an error when materializing a clean closure whose method has changed bytecodes.
	Send #yourself to prevent ConstantBlockClosure optmization."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ self yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self deny: aClosure isClean.
	self assert: aClosure outerContext isNotNil.

	self serialize: aClosure.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ self'
		in: aClass.
	self should: [ self materialized ] raise: FLMethodChanged.
	
	self materializer disableMethodChangedWarning.
	self materialized
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeSameBytecodesClean [
	"Tolerate materializing a clean closure whose method has changed but not the bytecodes.
	Send #yourself to prevent ConstantBlockClosure optmization."

	| aClass aClosure materializedClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 41 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertCleanBlockOuterContextBasedOnCompilationOption: aClosure.
	
	self serialize: aClosure.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ [ 42 yourself]'
		in: aClass.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.
	self assert: materializedClosure value equals: 42
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeSameBytecodesConstant [
	"Tolerate materializing a clean closure whose method has changed but not the bytecodes.
	In constant closure, the closure is replaced by the literal, so when
	the literal changes (bytecodes would still be identical) users should be warned."

	| aClass aClosure materializedClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 41 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assert: aClosure outerContext isNil.
	self assert: aClosure class equals: (Smalltalk at: #ConstantBlockClosure).
	
	self serialize: aClosure.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ 42'
		in: aClass.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.
	self assert: materializedClosure value equals: 41
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureChangeSameBytecodesFull [
	"Tolerate materializing a clean closure whose method has changed but not the bytecodes.
	Send #yourself to prevent ConstantBlockClosure optmization."

	| aClass aClosure materializedClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ self yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self deny: aClosure isClean.
	self assert: aClosure outerContext isNotNil.
	
	self serialize: aClosure.
	self classFactory
		silentlyCompile:  'methodWithClosure  ^ [ self yourself]'
		in: aClass.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.
	self assert: materializedClosure value class equals: aClass
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureMaterializesClassVariablesCorrectly [

	| class closure method |
	class := self classFactory silentlyMake: [ :aBuilder | aBuilder sharedVariables: #(ClassVariableForTesting)].
	self classFactory silentlyCompile: 'methodWithClosure  ^ [ ClassVariableForTesting ]' in: class.

	method := class methodNamed: #methodWithClosure.
	"Make sure we don't use global clusters here, which would simply perform a lookup"
	self serializer fullySerializeBehavior: method methodClass.
	closure := method valueWithReceiver: class new.

	self serialize: closure.

	self assert: (self materialized compiledBlock literalAt: 1) equals: (class bindingOf: 'ClassVariableForTesting').
	self deny: (self materialized compiledBlock literalAt: 1) identicalTo: (class bindingOf: 'ClassVariableForTesting')
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureRemovedClean [
	"Raise an error when materializing a clean closure whose method was removed.
	Send #yourself to prevent ConstantBlockClosure optmization."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 yourself ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assertCleanBlockOuterContextBasedOnCompilationOption: aClosure.
	
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodNotFound
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureRemovedConstant [
	"Raise no error when materializing a constant closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ 42 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self assert: aClosure isClean.
	self assert: aClosure outerContext isNil.
	self assert: aClosure class equals: (Smalltalk at: #ConstantBlockClosure).
	
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodNotFound
]

{ #category : 'tests-change' }
FLBlockClosureSerializationTest >> testBlockClosureRemovedFull [
	"Raise an error when materializing a non-clean closure whose method was removed."

	| aClass aClosure |
	aClass := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'methodWithClosure  ^ [ self + 1 ]'
		in: aClass.
	aClosure := aClass new perform: #methodWithClosure.
	self deny: aClosure isClean.
	self assert: aClosure outerContext isNotNil.
	
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodNotFound
]

{ #category : 'tests-clean' }
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

{ #category : 'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithGlobalVariable [
	| closure materializedClosure |
	closure := [ Smalltalk ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value equals: Smalltalk
]

{ #category : 'tests-unclean' }
FLBlockClosureSerializationTest >> testBlockClosureWithSelfSend [
	| closure materializedClosure |
	closure := self class blockClosureWithSelfSend.
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value equals: closure value
]

{ #category : 'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithTempVariableRead [
	| closure materializedClosure |
	closure := self class blockClosureWithTempVariableRead.
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value equals: 'TEST'
]

{ #category : 'tests-clean' }
FLBlockClosureSerializationTest >> testBlockClosureWithThreeArguments [
	| closure materializedClosure |
	closure := [ :a :b :c | a + b + c ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: (materializedClosure value: 1 value: 2 value: 3) equals: 6
]

{ #category : 'tests' }
FLBlockClosureSerializationTest >> testCleanBlockClosure [

	| aClosure meterialized |
	CompilationContext optionCleanBlockClosure ifFalse: [ ^ self skip ].

	aClosure := [ :x | x objects detect: [ :y | y isInteger ] ].
	self assert: aClosure isClean.
	self assert: aClosure outerContext isNil.

	self serializer addPostMaterializationAction: aClosure.
	self serialize: 87.

	self materializer disableMethodChangedWarning.
	meterialized := self materialized.

	self assert: meterialized equals: 87
]

{ #category : 'tests-clean' }
FLBlockClosureSerializationTest >> testNestedBlockClosureConstant [
	| closure materializedClosure |
	closure := [ [ 42 ] ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value value equals: 42
]

{ #category : 'tests' }
FLBlockClosureSerializationTest >> testNestedCleanBlockClosure [

	| aClosure meterialized |
	CompilationContext optionCleanBlockClosure ifFalse: [ ^ self skip ].

	aClosure := [ [ :x | x objects detect: [ :y | y isInteger ] ] ] value.
	self assert: aClosure isClean.
	self assert: aClosure outerContext isNil.

	self serializer addPostMaterializationAction: aClosure.
	self serialize: 87.

	self materializer disableMethodChangedWarning.
	meterialized := self materialized.

	self assert: meterialized equals: 87
]
