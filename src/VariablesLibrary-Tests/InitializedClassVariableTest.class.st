Class {
	#name : #InitializedClassVariableTest,
	#superclass : #SlotSilentTest,
	#category : #'VariablesLibrary-Tests-Tests'
}

{ #category : #tests }
InitializedClassVariableTest >> testInitializedClassVariableCompiled [
	| classVar |
	classVar := #ClassVar => InitializedClassVariable default: 5.
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self compileAccessorsFor: classVar.
	self assert: aClass new ClassVar equals: 5.
	aClass new ClassVar: 6.
	self assert: aClass new ClassVar equals: 6
	
	

	
]

{ #category : #tests }
InitializedClassVariableTest >> testInitializedClassVariableReadReflective [
	| classVar |
	classVar := #ClassVar => InitializedClassVariable default: 5.
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: 5.
	classVar write: 10.
	self assert: classVar read equals: 10
	
	
	
	
	
]

{ #category : #tests }
InitializedClassVariableTest >> testInitializedClassVariableReflectiveBlock [
	| classVar |
	classVar := #ClassVar => InitializedClassVariable default: [5].
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: 5.
	classVar write: 10.
	self assert: classVar read equals: 10
	
	
	
	
	
]

{ #category : #tests }
InitializedClassVariableTest >> testInitializedClassVariableReflectiveBlockParameter [
	| classVar |
	classVar := #ClassVar => InitializedClassVariable default: [:class | class].
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: aClass
]
