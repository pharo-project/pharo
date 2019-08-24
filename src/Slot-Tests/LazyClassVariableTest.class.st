Class {
	#name : #LazyClassVariableTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVarCompiled [
	| classVar |
	
	classVar := #ClassVar => LazyClassVariable default: 5.
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self compileAccessorsFor: classVar.
	self assert: aClass new ClassVar equals: 5.
	aClass new ClassVar: 6.
	self assert: aClass new ClassVar equals: 6.
	
	

	
]

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVarReadReflective [
	| classVar |
	classVar := #ClassVar => LazyClassVariable default: 5.
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: 5.
	classVar write: 10.
	self assert: classVar read equals: 10.
	
	
	
	
	
]

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVarReadReflectiveBlock [
	| classVar |
	classVar := #ClassVar => LazyClassVariable defaultBlock: [5].
	aClass := self make: [ :builder | builder sharedVariables: {classVar}].

	self assert: classVar read equals: 5.
	classVar write: 10.
	self assert: classVar read equals: 10.
	
	
	
	
	
]

{ #category : #tests }
LazyClassVariableTest >> testLazyClassVariablePrint [
	| slot |
	slot := #myClassVar => LazyClassVariable default: 5 .
	self assert: slot printString equals: '#myClassVar => LazyClassVariable default: 5'.
	slot := #myClassVar => LazyClassVariable defaultBlock: [5].
	self assert: slot printString equals: '#myClassVar => LazyClassVariable defaultBlock: [ 5 ]'.
	"if the default is really a block"
	slot := #myClassVar => LazyClassVariable default: [5].
	self assert: slot printString equals: '#myClassVar => LazyClassVariable default: [ 5 ]' 
]
