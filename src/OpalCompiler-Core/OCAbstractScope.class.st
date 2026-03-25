"
I am a symbol table where variable names are associated with semantics variables.  Each context (method/closure) gets a fresh scope that inherits from its outer scope.
"
Class {
	#name : 'OCAbstractScope',
	#superclass : 'Object',
	#instVars : [
		'outerScope'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'testing' }
OCAbstractScope class >> isAbstract [
	^self = OCAbstractScope
]

{ #category : 'accessing' }
OCAbstractScope >> environment [
	^self targetClass environment
]

{ #category : 'testing' }
OCAbstractScope >> hasTempVector [
	^ false
]

{ #category : 'testing' }
OCAbstractScope >> isBlockScope [

	^false
]

{ #category : 'testing' }
OCAbstractScope >> isInsideOptimizedLoop [
	^ false
]

{ #category : 'testing' }
OCAbstractScope >> isMethodScope [

	^false
]

{ #category : 'lookup' }
OCAbstractScope >> lookupVar: name [
	"search the scope (and the outer scopes) for a variable 'name' and return it"

	^ outerScope ifNotNil: [ :it | it lookupVar: name ]
]

{ #category : 'testing' }
OCAbstractScope >> nonOptimizedDistanceTo: aDefiningScope [
	
	| currentScope distance |
	currentScope := self.
	distance := 0.

	[ currentScope ~= aDefiningScope ] whileTrue: [
		(currentScope isBlockScope and: [ currentScope isOptimized not ])
			ifTrue: [ distance := distance + 1 ].
		currentScope := currentScope outerScope ].

	^ distance
]

{ #category : 'lookup' }
OCAbstractScope >> outerScope [

	^ outerScope
]

{ #category : 'initialization' }
OCAbstractScope >> outerScope: aSemScope [

	outerScope := aSemScope
]

{ #category : 'lookup' }
OCAbstractScope >> registerVariables [

	outerScope ifNotNil: [ :it | it registerVariables ]
]

{ #category : 'accessing' }
OCAbstractScope >> targetClass [
	^outerScope ifNotNil: [ :it | it targetClass ]
]
