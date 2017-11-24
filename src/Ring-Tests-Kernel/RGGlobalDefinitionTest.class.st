"
SUnit tests for global definitions (pools, global variables)
"
Class {
	#name : #RGGlobalDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Tests-Kernel'
}

{ #category : #testing }
RGGlobalDefinitionTest >> testEquality [
	| global1 global2 |
	
	global1 := RGGlobalVariableDefinition named: #Transcript.
	global2 := RGGlobalVariableDefinition named: #Transcript.
	self assert: global1 = global2.
	
	global2 := RGGlobalVariableDefinition named: #Transcript2.
	self deny: global1 = global2.
]

{ #category : #testing }
RGGlobalDefinitionTest >> testGlobalVariableDefinition [
	| global  |
	
	global:= RGGlobalVariableDefinition named:  #Transcript.
	self assert: (global isGlobalVariable).
	self assert: (global parent = Smalltalk globals).
]

{ #category : #testing }
RGGlobalDefinitionTest >> testPoolDefinition [
	| pool newClass |
	pool := RGClassDefinition createSharedPoolNamed: #TextConstants.
	self assert: pool isPool.
	self assert: pool users isEmpty.
	self assert: pool parent equals: Smalltalk globals.
	newClass := RGClassDefinition named: #OrderedCollection.
	pool addUser: newClass.
	self assert: pool users size equals: 1.
	self assert: (pool includesUser: newClass).
	self assert: (newClass sharedPoolNamed: #TextConstants) isPoolVariable.
	pool := RGClassDefinition named: #TextConstants.
	pool superclassName: #SharedPool.
	self assert: pool isPool.
	self assert: pool users isEmpty
]
