Class {
	#name : #OCASTVariableTranslatorTest,
	#superclass : #OCASTTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignArgumentVariable [
	self
		should: [self compileSource: 'argument: anArgument
	anArgument := 17'] raise: OCStoreIntoReadOnlyVariableError
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignClassVariable [
	self assert: instance iVar equals: nil.
	self testExample: #exampleClassVariableAssignment.
	self assert: instance classVariable equals: 1
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignGlobalVariable [
	globals add: (GlobalVariable key: #GlobalVariable value: nil).
	self testSource: 'exampleGlobalVariableAssignment
	GlobalVariable := 17'.
	self assert: (globals at: #GlobalVariable) equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignInstanceVariable [
	self assert: instance iVar equals: nil.
	self testExample: #exampleInstanceVariableAssignment.
	self assert: instance iVar equals: 1
	
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignSelfVariable [
	self
		should: [self compileSource: 'foo
	self := 17'] raise: OCStoreIntoSpecialVariableError
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignSuperVariable [
	self
		should: [self compileSource: 'foo
	super := 17'] raise: OCStoreIntoSpecialVariableError
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignTemporaryVariable [
	self testExample: #exampleTemporaryVariableAssignment.
	
	self assert: instance result equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testAssignThisContextVariable [
	self
		should: [self compileSource: 'foo
	thisContext := 17'] raise: OCStoreIntoSpecialVariableError
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushArgumentVariable [
	self testExample: #examplePushArgumentVariable: withArgument: 17.
	
	self assert: instance result equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushClassVariable [
	instance classVariable: 17.
	self testExample: #examplePushClassVariable.
	
	self assert: instance result equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushGlobalVariable [

	globals add: (GlobalVariable key: #GlobalVariable value: 17).
	self testSource: 'exampleGlobalVariableAssignment
	self message: GlobalVariable'.
	
	self assert: instance result equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushInstanceVariable [
	instance iVar: 17.
	self testExample: #examplePushInstanceVariable.
	
	self assert: instance result equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushSelf [
	
	self assert: instance result equals: nil.
	self testExample: #exampleSelf.
	self assert: instance result equals: instance
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushSuper [
	
	self assert: instance result equals: nil.
	self testExample: #exampleSuper.
	self assert: instance result equals: instance
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushTemporaryVariable [
	instance iVar: 17.
	self testExample: #examplePushTemporaryVariable.
	
	self assert: instance result equals: 17
]

{ #category : #'testing - variables' }
OCASTVariableTranslatorTest >> testPushThisContext [
	
	| method |
	method := self compileExample: #exampleThisContext.
	self executeMethodOnInstance: method.

	self assert: instance result isContext.
	self assert: instance result method equals: method.	
]
