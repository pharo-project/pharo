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
OCRequestorScope >> findVariable: aBlock ifNone: aNotFound [
	outerScope ifNotNil: [
		^  outerScope findVariable: aBlock ifNone: aNotFound ]
]

{ #category : #lookup }
OCRequestorScope >> lookupVar: name [

	name = 'self' ifTrue: [ ^ outerScope lookupVar: name ].
	name = 'super' ifTrue: [ ^ outerScope lookupVar: name ].
	name first isUppercase ifTrue: [ ^ outerScope lookupVar: name].
	
	"do not 'create bindings' in requestor scope if we just want to style a possible unknown variable"
	(compilationContext optionSkipSemanticWarnings
		and: [ (requestor hasBindingOf: name asSymbol) not ])
		ifTrue: [ ^ outerScope lookupVar: name ].

	"the requestors #bindingOf may create a binding for not yet existing variables"
	(requestor bindingOf: name asSymbol)
		ifNotNil: [ :assoc | 
			^ OCLiteralVariable new
				assoc: assoc;
				scope: self;
				yourself ].
	^ outerScope lookupVar: name
]

{ #category : #lookup }
OCRequestorScope >> newMethodScope [
	 
	^ OCMethodScope new outerScope: (self outerScope: outerScope instanceScope) 
]

{ #category : #accessing }
OCRequestorScope >> requestor [
	^ requestor
]

{ #category : #accessing }
OCRequestorScope >> requestor: anObject [
	requestor := anObject
]
