"
This class models a scope for the workspace (and the debugger and all other tools).

The default scope of the compiler is initialized with a Requestor scope, if the requestor is not nil (see CompilationContext>>scope)

The OCRequestorScope will ask the tool (the requestor) for bindings. This will be an association, and as such it will create a OCLiteralVariable.  It will compile the same bytecode as for a global, but it will use the associations hold on by the tool to do so.
"
Class {
	#name : #OCRequestorScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'requestor',
		'compilationContext',
		'variables'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'temp vars' }
OCRequestorScope >> allTemps [
	^#()
]

{ #category : #accessing }
OCRequestorScope >> compilationContext [
	^ compilationContext
]

{ #category : #accessing }
OCRequestorScope >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : #lookup }
OCRequestorScope >> lookupVar: name [

	(requestor hasBindingOf: name) ifTrue: [ ^ requestor bindingOf: name ].
	"Note that is an outerscope is a requestor tant can declare, it will win the declaration.
	But maybe it is better this way."
	(outerScope lookupVar: name) ifNotNil: [ :binding | ^ binding ].
	
	"Heuristic, assume the requestor does not want upercased variables.
	A menu will likely open to offer reparation for class or global or something."
	name first isUppercase ifTrue: [ ^ nil ].
	
	"We do not register it yet, to not fill the requestor with garbage variable during styling for instance"
	^ self variables at: name ifAbsentPut: [ WorkspaceVariable key: name ]
]

{ #category : #lookup }
OCRequestorScope >> registerVariables [

	self variables do: [ :each |
		| var |
		"Force the declaraction, if needed"
		var := requestor bindingOf: each name.
		"Because we no non control how requestor handle its variables, just update the one we used locally"
		var == each ifFalse: [ each becomeForward: var ] ].
	super registerVariables
]

{ #category : #accessing }
OCRequestorScope >> requestor [
	^ requestor
]

{ #category : #accessing }
OCRequestorScope >> requestor: anObject [
	requestor := anObject
]

{ #category : #accessing }
OCRequestorScope >> variables [

	^ variables ifNil: [ variables := Dictionary new ].
]
