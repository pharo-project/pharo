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
	"Answer the value of the temp at index in aContext where index is relative"
	
	^self tempNamed: (aContext tempNames at: index) in: aContext
]

{ #category : #public }
DebuggerMethodMapOpal >> namedTempAt: index put: aValue in: aContext [
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names
	 If the value is a copied value we also need to set it along the lexical chain."
	
	^self tempNamed: (aContext tempNames at: index) in: aContext put: aValue
]

{ #category : #public }
DebuggerMethodMapOpal >> tempNamed: name in: aContext [
	"Answer the value of the temp with name in aContext"
	
	| scope var |
	scope := aContext sourceNodeExecuted scope.
	var := scope lookupVar: name.
	^var readFromContext: aContext scope: scope.
		
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
