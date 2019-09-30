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

{ #category : #public }
DebuggerMethodMapOpal >> namedTempAt: index in: aContext [
	"please use #namedTempAt: on Context"
	^aContext namedTempAt: index
]

{ #category : #public }
DebuggerMethodMapOpal >> namedTempAt: index put: aValue in: aContext [
	"please use #namedTempAt:put: on Context"
	^aContext namedTempAt: index put: aValue
]

{ #category : #initialization }
DebuggerMethodMapOpal >> rangeForPC: aPC contextIsActiveContext: aBoolean [
	"please use pcRangeContextIsActive: on Context"
	"return the debug highlight for aPC"		
	| pc | 			

  	"When on the top of the stack the pc is pointing to right instruction, but deeper in the stack		
 	the pc was already advanced one bytecode, so we need to go back this one bytecode, which		
 	can consist of multiple bytes. But on IR, we record the *last* bytecode offset as the offset of the		
 	IR instruction, which means we can just go back one"		
	
	pc := aBoolean ifTrue: [aPC] ifFalse: [aPC - 1].		
 	^(methodNode sourceNodeForPC: pc) debugHighlightRange
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamed: name in: aContext [
	"please use #tempNamed: on Context"
	^aContext tempNamed: name
		
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamed: name in: aContext put: aValue [
	"please use #tempNamed:put: on Context"
	^aContext tempNamed: name put: aValue
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamesForContext: aContext [
	"use tempNames on Context"
	^ aContext tempNames 
]
