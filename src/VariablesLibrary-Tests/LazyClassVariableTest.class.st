Class {
	#name : #LazyClassVariableTest,
	#superclass : #SlotSilentTest,
	#category : #'VariablesLibrary-Tests-Tests'
}

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVarCompiled [
	| classVar |
	classVar := #ClassVar => LazyClassVariable default: 5.
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self compileAccessorsFor: classVar.
	self assert: aClass new ClassVar equals: 5.
	aClass new ClassVar: 6.
	self assert: aClass new ClassVar equals: 6
	
	

	
]

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVarReadReflective [
	| classVar |
	classVar := #ClassVar => LazyClassVariable default: 5.
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: 5.
	classVar write: 10.
	self assert: classVar read equals: 10
	
	
	
	
	
]

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVarReadReflectiveBlock [
	| classVar |
	classVar := #ClassVar => LazyClassVariable default: [5].
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: 5.
	classVar write: 10.
	self assert: classVar read equals: 10
	
	
	
	
	
]

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVariablePrint [
	| classVariable |
	classVariable := #myClassVar => LazyClassVariable default: 5 .
	self assert: classVariable printString equals: '#myClassVar => LazyClassVariable default: 5'.
	classVariable := #myClassVar => LazyClassVariable default: [5].
	self assert: classVariable printString equals: '#myClassVar => LazyClassVariable default: [ 5 ]'.
	"if the default is really a block"
	classVariable := #myClassVar => LazyClassVariable default: [5].
	self assert: classVariable printString equals: '#myClassVar => LazyClassVariable default: [ 5 ]' 
]
