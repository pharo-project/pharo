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
		'compilationContext'
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
OCRequestorScope >> lookupVar: name declare: aBoolean [
	
	"generated temps (e.g. for limits in to:do: should not create bindings"
	name first = $0 ifTrue: [ ^ outerScope lookupVar: name declare: aBoolean ].
	"We do not want to auto define bindings for unknown Globals"
	(name isString and: [name first isUppercase]) ifTrue: [ ^ outerScope lookupVar: name declare: aBoolean].
	"do not 'create bindings' in requestor scope if we just want to style a possible unknown variable"
	((compilationContext optionSkipSemanticWarnings or: [aBoolean not])
		and: [ (requestor hasBindingOf: name) not ])
		ifTrue: [ ^ outerScope lookupVar: name declare: aBoolean ].

	"the requestors #bindingOf may create a binding for not yet existing variables"
	^(requestor bindingOf: name)
		ifNotNil: [ :var | var ]
		ifNil: [ outerScope lookupVar: name declare: aBoolean ]
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
OCRequestorScope >> scopeLevel [
	^ 0
]
