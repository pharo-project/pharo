"
I am a symbol table where variable names are associated with SemVars.  Each context (method/closure) get a fresh scope that inherits from its outer scope.
"
Class {
	#name : #OCAbstractScope,
	#superclass : #Object,
	#instVars : [
		'outerScope',
		'node',
		'children'
	],
	#category : #OpalCompiler-Core-Semantics
}

{ #category : #initializing }
OCAbstractScope >> addChild: aScope [
	self children add: aScope
]

{ #category : #accessing }
OCAbstractScope >> children [
	^children ifNil: [ children := OrderedCollection new ]
]

{ #category : #testing }
OCAbstractScope >> hasTempVector [
	^ false
]

{ #category : #decompiling }
OCAbstractScope >> instanceScope [

	^self outerScope instanceScope
]

{ #category : #testing }
OCAbstractScope >> isBlockScope [

	^false
]

{ #category : #testing }
OCAbstractScope >> isInsideOptimizedLoop [
	^ false
]

{ #category : #testing }
OCAbstractScope >> isInstanceScope [
	^ false
]

{ #category : #testing }
OCAbstractScope >> isMethodScope [

	^false
]

{ #category : #testing }
OCAbstractScope >> isOptimizedBlockScope [
	^false
]

{ #category : #lookup }
OCAbstractScope >> lookupSelector: name [

	Symbol hasInterned: name ifTrue: [ :sym | ^ sym].
	^ nil
]

{ #category : #lookup }
OCAbstractScope >> lookupVar: name [
	"subclass responsibility"

	^ self outerScope lookupVar: name
]

{ #category : #lookup }
OCAbstractScope >> lookupVar: name inContext: aContext [

	^ self outerScope lookupVar: name inContext: aContext outerContext
]

{ #category : #lookup }
OCAbstractScope >> lookupVarForDeclaration: name [
	"subclass responsibility"

	^ self outerScope lookupVarForDeclaration: name
]

{ #category : #accessing }
OCAbstractScope >> node [
	^node
]

{ #category : #accessing }
OCAbstractScope >> node: aNode [
	node := aNode
]

{ #category : #levels }
OCAbstractScope >> outerScope [

	^ outerScope
]

{ #category : #initializing }
OCAbstractScope >> outerScope: aSemScope [

	outerScope := aSemScope.
	aSemScope addChild: self. 
]

{ #category : #lookup }
OCAbstractScope >> possibleSelectorsFor: string [

	^ Symbol possibleSelectorsFor: string
]

{ #category : #levels }
OCAbstractScope >> scopeLevel [

	outerScope ifNil: [^ 0].
	^ outerScope scopeLevel + 1
]

{ #category : #accessing }
OCAbstractScope >> variableNamed: aName [

	^ self variableNamed: aName ifAbsent: [ nil ]
]
