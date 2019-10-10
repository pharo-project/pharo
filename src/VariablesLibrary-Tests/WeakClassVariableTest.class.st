Class {
	#name : #WeakClassVariableTest,
	#superclass : #SlotSilentTest,
	#category : #'VariablesLibrary-Tests-Tests'
}

{ #category : #tests }
WeakClassVariableTest >> testCreateClassWithWeakClassVariable [
	| classVar |
	
	"Add class variable using the builder interface"
	classVar := #ClassVar => WeakClassVariable.
	aClass := self make: [ :builder | builder sharedVariables: {classVar} ].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar')
	
	
]

{ #category : #tests }
WeakClassVariableTest >> testReadWriteWeakClassVarCompiled [
	| classVar object valuetoTest |
	classVar := #ClassVar => WeakClassVariable.
	aClass := self make: [ :builder | builder sharedVariables: {classVar} ].
	
	object := aClass new.
	
	"now compile an accessor and read"
	self compileAccessorsFor: classVar.
	self assert: object ClassVar equals: nil.
	
	valuetoTest := Object new.
	
	object ClassVar: valuetoTest.
	self assert: object ClassVar equals: valuetoTest
]

{ #category : #tests }
WeakClassVariableTest >> testWeakClassVariableIsWeak [
	| classVar |
	classVar := #ClassVar => WeakClassVariable.
	aClass := self make: [ :builder | builder sharedVariables: {classVar} ].
	
	classVar write: Object new.
	Smalltalk garbageCollect.
	"the value is GCed and we get nil"
	self assert: classVar read equals: nil
]

{ #category : #tests }
WeakClassVariableTest >> testWeakClassVariableReflective [
	| classVar |
	classVar := #ClassVar => WeakClassVariable.
	aClass := self make: [ :builder | builder sharedVariables: {classVar} ].
	
	classVar write: 5.
	self assert: classVar read equals: 5
]
