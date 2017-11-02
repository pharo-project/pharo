"
I provide helper methods deadling with

-> pc to text mapping
-> temporary variables for contexts
-> reading and setting tempary variables

All methods here should be moved to MethodContext.
"
Class {
	#name : #DebuggerMethodMapOpal,
	#superclass : #Object,
	#instVars : [
		'methodNode'
	],
	#category : 'OpalCompiler-Core-FrontEnd'
}

{ #category : #'instance creation' }
DebuggerMethodMapOpal class >> forMethod: aMethod [ "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod"
	^self new forMethod: aMethod
	
				
]

{ #category : #initialize-release }
DebuggerMethodMapOpal >> forMethod: aCompiledMethod [ 
	methodNode := aCompiledMethod ast
]

{ #category : #public }
DebuggerMethodMapOpal >> namedTempAt: index in: aContext [
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"

	| name |	
	name := (self tempNamesForContext: aContext) at: index.
	^self tempNamed: name in: aContext.
]

{ #category : #public }
DebuggerMethodMapOpal >> namedTempAt: index put: aValue in: aContext [
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:.
	 If the value is a copied value we also need to set it along the lexical chain."
	
	| name |	
	name := (self tempNamesForContext: aContext) at: index.
	^self tempNamed: name in: aContext put: aValue.
]

{ #category : #public }
DebuggerMethodMapOpal >> rangeForPC: aPC [
	"return the debug highlight for aPC"
	
	^self rangeForPC: aPC contextIsActiveContext: false
	
]

{ #category : #public }
DebuggerMethodMapOpal >> rangeForPC: aPC contextIsActiveContext: contextIsActive [
	"return the debug highlight for aPC"
	| pc |
	
	"When on the top of the stack the pc is pointing to right instruction, but deeper in the stack
	the pc was already advanced one bytecode, so we need to go back this one bytecode, which
	can consist of multiple bytes. But on IR, we record the *last* bytecode offset as the offset of the
	IR instruction, which means we can just go back one"

	pc := contextIsActive ifTrue: [aPC] ifFalse: [aPC - 1].
	^(methodNode sourceNodeForPC:  pc) debugHighlightRange
	
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamed: name in: aContext [
	"Answer the value of the temp with name in aContext"
	
	| scope var |
	scope := aContext sourceNodeExecuted scope.
	var := scope lookupVar: name.
	^var searchFromContext: aContext scope: scope.
		
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamed: name in: aContext put: aValue [
	"Assign the value of the temp with name in aContext
	 If the value is a copied value we also need to set it along the lexical chain."

	| scope var |
	scope := aContext sourceNodeExecuted scope.
	var := scope lookupVar: name.
	^var writeFromContext: aContext scope: scope value: aValue.
	
	
	
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamesForContext: aContext [
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	
	^ aContext sourceNodeExecuted scope allTempNames.
]
