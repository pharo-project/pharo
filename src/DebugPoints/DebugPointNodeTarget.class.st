"
I am a debug point target that installs a metalink on an AST node of a method
"
Class {
	#name : 'DebugPointNodeTarget',
	#superclass : 'DebugPointClassTarget',
	#category : 'DebugPoints-Implementations',
	#package : 'DebugPoints',
	#tag : 'Implementations'
}

{ #category : 'actions' }
DebugPointNodeTarget >> browse [

	^ self method browse
]

{ #category : 'private ' }
DebugPointNodeTarget >> install: aLink on: aClassOrObject [

	aClassOrObject link: aLink toAST: self node
]

{ #category : 'accessing' }
DebugPointNodeTarget >> method [
	"returns the method to which the debug point is attached"

	^ self methodNode compiledMethod
]

{ #category : 'accessing' }
DebugPointNodeTarget >> methodNode [

	^ self node methodNode
]

{ #category : 'accessing' }
DebugPointNodeTarget >> node [

	^ self target
]

{ #category : 'accessing' }
DebugPointNodeTarget >> node: aNode [

	self target: aNode
]

{ #category : 'accessing' }
DebugPointNodeTarget >> sourceCode [

	^ self methodNode sourceCode
]

{ #category : 'accessing' }
DebugPointNodeTarget >> targetClass [

	^ self method methodClass
]

{ #category : 'description' }
DebugPointNodeTarget >> targetString [

	^ self method printString
]
