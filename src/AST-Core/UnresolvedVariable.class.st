"
The parser does not now the semantic meaning of Variables. It thus sets them all to be an UnresolvedVariable.

The Semantic Analysis (#doSemanticAnalysis) then gives meaning to Variables and sets them correctly.
"
Class {
	#name : #UnresolvedVariable,
	#superclass : #Variable,
	#classInstVars : [
		'instance'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #accessing }
UnresolvedVariable class >> instance [
	^instance ifNil: [ instance := self new ]
]

{ #category : #'code generation' }
UnresolvedVariable >> emitStore: methodBuilder [

	self error: 'Can not generate code, please call #doSemanticAnalysis first'
]

{ #category : #'code generation' }
UnresolvedVariable >> emitValue: methodBuilder [

	self error: 'Can not generate code, please call #doSemanticAnalysis first'
]

{ #category : #debugging }
UnresolvedVariable >> readInContext: aContext [
	self error: 'Can not read variable, please call #doSemanticAnalysis first'
]

{ #category : #accessing }
UnresolvedVariable >> scope [
	^nil
]

{ #category : #queries }
UnresolvedVariable >> usingMethods [
	^#()
]

{ #category : #debugging }
UnresolvedVariable >> write: aValue inContext: aContext [
	self error: 'Can not write variable, please call #doSemanticAnalysis first'
]
