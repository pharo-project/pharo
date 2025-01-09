"
This class models a scope for the workspace (and the debugger and all other tools).

The default scope of the compiler is initialized with a Requestor scope, if the requestor is not nil (see `CompilationContext>>#scope`).

The `OCRequestorScope` will ask the tool (the requestor) for bindings. This will be an association, and as such it will create a `OCLiteralVariable`.  It will compile the same bytecode as for a global, but it will use the associations hold on by the tool to do so.
"
Class {
	#name : 'OCRequestorScope',
	#superclass : 'OCAbstractScope',
	#instVars : [
		'requestor',
		'variables'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'temp vars' }
OCRequestorScope >> allTemps [
	^#()
]

{ #category : 'testing' }
OCRequestorScope >> canAddBindingOf: name [
	"Test for free workspace declaration"
	
	"MD: Question: Do we really need to delegate this to the requestor?"
	
	"Empty names come from faulty parameters. Ignore them"
	name ifEmpty: [ ^ false ].
	"names startign with digits are variables from optimized contructs"
	name startsWithDigit ifTrue: [ ^false ].

	"Because requestor do not have a real API, introspection is used."
	(requestor respondsTo: #canAddBindingOf:) ifFalse: [ ^ false ].
	^ requestor canAddBindingOf: name
]

{ #category : 'lookup' }
OCRequestorScope >> lookupVar: name [

	(requestor hasBindingOf: name) ifTrue: [ ^ requestor bindingOf: name ].
	"Note that is an outerscope is a requestor tant can declare, it will win the declaration.
	But maybe it is better this way."
	(super lookupVar: name) ifNotNil: [ :binding | ^ binding ].

	"Can we have a free workspace variable or not?"
	(self canAddBindingOf: name) ifFalse: [ ^nil ].
	"We do not register it yet, to not fill the requestor with garbage variable during styling for instance
	We use an UndeclaredVariable to color correctly. #registerVariables changes it to a WorkspaceVariable"
	^ self variables at: name ifAbsentPut: [ UndeclaredVariable named: name ]
]

{ #category : 'lookup' }
OCRequestorScope >> registerVariables [

	self variables do: [ :each |
		| var |
		"Force the declaraction, if needed"
		var := requestor bindingOf: each name.
		"Because we no non control how requestor handle its variables, just update the one we used locally"
		(var isNotNil and: [ var ~~ each ]) ifTrue: [ each becomeForward: var ] ].
	super registerVariables
]

{ #category : 'accessing' }
OCRequestorScope >> requestor [
	^ requestor
]

{ #category : 'accessing' }
OCRequestorScope >> requestor: anObject [
	requestor := anObject
]

{ #category : 'accessing' }
OCRequestorScope >> variables [

	^ variables ifNil: [ variables := Dictionary new ].
]
