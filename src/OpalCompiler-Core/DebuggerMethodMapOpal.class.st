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
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #'instance creation' }
DebuggerMethodMapOpal class >> forMethod: aMethod [ "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod"
	^self new forMethod: aMethod
	
				
]

{ #category : #initialization }
DebuggerMethodMapOpal >> forMethod: aCompiledMethod [ 
	methodNode := aCompiledMethod ast
]
