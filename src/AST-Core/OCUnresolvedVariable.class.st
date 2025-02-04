"
The parser does not now the semantic meaning of Variables. It thus sets them all to be an UnresolvedVariable.

The Semantic Analysis (#doSemanticAnalysis) then gives meaning to Variables and sets them correctly.
"
Class {
	#name : 'OCUnresolvedVariable',
	#superclass : 'Variable',
	#classInstVars : [
		'instance'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
OCUnresolvedVariable class >> instance [
	^instance ifNil: [ instance := self new ]
]

{ #category : 'code generation' }
OCUnresolvedVariable >> emitStore: methodBuilder [

	self error: 'Can not generate code, please call #doSemanticAnalysis first'
]

{ #category : 'code generation' }
OCUnresolvedVariable >> emitValue: methodBuilder [

	self error: 'Can not generate code, please call #doSemanticAnalysis first'
]

{ #category : 'debugging' }
OCUnresolvedVariable >> readInContext: aContext [
	self error: 'Can not read variable, please call #doSemanticAnalysis first'
]

{ #category : 'accessing' }
OCUnresolvedVariable >> scope [
	^nil
]

{ #category : 'queries' }
OCUnresolvedVariable >> usingMethods [
	^#()
]

{ #category : 'debugging' }
OCUnresolvedVariable >> write: aValue inContext: aContext [
	self error: 'Can not write variable, please call #doSemanticAnalysis first'
]
