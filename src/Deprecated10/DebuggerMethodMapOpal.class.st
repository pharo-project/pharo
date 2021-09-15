"
I provide helper methods deadling with

-> pc to text mapping
-> temporary variables for contexts
-> reading and setting tempary variables

All methods here habe been moved to Context
"
Class {
	#name : #DebuggerMethodMapOpal,
	#superclass : #Object,
	#instVars : [
		'methodNode'
	],
	#category : #Deprecated10
}

{ #category : #'instance creation' }
DebuggerMethodMapOpal class >> forMethod: aMethod [ "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod"
	^self new forMethod: aMethod
	
				
]

{ #category : #initialization }
DebuggerMethodMapOpal >> forMethod: aCompiledMethod [
	self deprecated: 'please use the reflactive API directly to read variables!'.
	methodNode := aCompiledMethod ast
]
