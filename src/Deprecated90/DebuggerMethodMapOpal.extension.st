Extension { #name : #DebuggerMethodMapOpal }

{ #category : #'*Deprecated90' }
DebuggerMethodMapOpal >> namedTempAt: index in: aContext [
	"please use #namedTempAt: on Context"
	self deprecated: 'Please use aContext namedTempAt: index'.
	^aContext namedTempAt: index
]

{ #category : #'*Deprecated90' }
DebuggerMethodMapOpal >> namedTempAt: index put: aValue in: aContext [
	"please use #namedTempAt:put: on Context"
	self deprecated: 'Please use aContext namedTempAt: index put: aValue'.
	aContext namedTempAt: index put: aValue
]

{ #category : #'*Deprecated90' }
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

{ #category : #'*Deprecated90' }
DebuggerMethodMapOpal >> tempNamed: name in: aContext [
	"please use #tempNamed: on Context"
	self deprecated: 'please use #tempNamed: on Context'.
	^aContext tempNamed: name
		
]

{ #category : #'*Deprecated90' }
DebuggerMethodMapOpal >> tempNamed: name in: aContext put: aValue [
	"please use #tempNamed:put: on Context"
	self deprecated: 'please use #tempNamed:put: on Context'.
	aContext tempNamed: name put: aValue
]

{ #category : #'*Deprecated90' }
DebuggerMethodMapOpal >> tempNamesForContext: aContext [
	"use tempNames on Context"
	self deprecated: 'use tempNames on Context'.
	^ aContext tempNames 
]
