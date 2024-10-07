"
Primitives can declare a local variable that will hold the error code.
"
Class {
	#name : 'PrimitiveErrorVariable',
	#superclass : 'LocalVariable',
	#instVars : [
		'node'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'instance creation' }
PrimitiveErrorVariable class >> node: aVariableNode [
	^self new
		name: aVariableNode name;
		node: aVariableNode;
		markWrite;
		yourself
]

{ #category : 'visiting' }
PrimitiveErrorVariable >> acceptVisitor: arg1 node: arg2 [

	^ arg1 visitTemporaryVariableNode: arg2
]

{ #category : 'queries' }
PrimitiveErrorVariable >> definingNode [

	^ node
]

{ #category : 'accessing' }
PrimitiveErrorVariable >> node: arg1 [

	node := arg1
]
