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
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #'instance creation' }
DebuggerMethodMapOpal class >> forMethod: aMethod [
	self deprecated: 'use the reflective API directly. See methods of DebuggerMethodMapOpal'.
	^self new forMethod: aMethod
	
				
]

{ #category : #initialization }
DebuggerMethodMapOpal >> forMethod: aCompiledMethod [ 
	self deprecated: 'use the reflective API directly. See methods of DebuggerMethodMapOpal'.
	methodNode := aCompiledMethod ast
]

{ #category : #'as yet unclassified' }
DebuggerMethodMapOpal >> namedTempAt: index in: aContext [
	"Please access temps by name using #tempNamed: on Context"
	self deprecated: 'Please access temps by name using #tempNamed: on Context'.
	^aContext tempNamed: (aContext tempNames at: index)
]

{ #category : #'as yet unclassified' }
DebuggerMethodMapOpal >> namedTempAt: index put: aValue in: aContext [
	"Please access temps by name using #tempNamed:put:"
	self deprecated: 'Please access temps by name using #tempNamed:put:'.
	aContext tempNamed: (aContext tempNames at: index) put: aValue
]

{ #category : #'as yet unclassified' }
DebuggerMethodMapOpal >> rangeForPC: aPC contextIsActiveContext: aBoolean [
	"please use pcRangeContextIsActive: on Context"

	"return the debug highlight for aPC"		
	| pc | 			
	self deprecated: 'please use pcRangeContextIsActive: on Context'.
  	"When on the top of the stack the pc is pointing to right instruction, but deeper in the stack		
 	the pc was already advanced one bytecode, so we need to go back this one bytecode, which		
 	can consist of multiple bytes. But on IR, we record the *last* bytecode offset as the offset of the		
 	IR instruction, which means we can just go back one"		
	
	pc := aBoolean ifTrue: [aPC] ifFalse: [aPC - 1].		
 	^(methodNode sourceNodeForPC: pc) debugHighlightRange
]

{ #category : #'as yet unclassified' }
DebuggerMethodMapOpal >> tempNamed: name in: aContext [
	"please use #tempNamed: on Context"
	self deprecated: 'please use #tempNamed: on Context'.
	^aContext tempNamed: name
		
]

{ #category : #'as yet unclassified' }
DebuggerMethodMapOpal >> tempNamed: name in: aContext put: aValue [
	"please use #tempNamed:put: on Context"
	self deprecated: 'please use #tempNamed:put: on Context'.
	aContext tempNamed: name put: aValue
]

{ #category : #'as yet unclassified' }
DebuggerMethodMapOpal >> tempNamesForContext: aContext [
	"use tempNames on Context"
	self deprecated: 'use tempNames on Context'.
	^ aContext tempNames 
]
