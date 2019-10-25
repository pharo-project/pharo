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
	#category : #'OpalCompiler-Core-Semantics'
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

{ #category : #lookup }
OCAbstractScope >> lookupVar: name [
	"search the scope (and the outer scopes) for a variable 'name' and return it"
	^self subclassResponsibility
]

{ #category : #lookup }
OCAbstractScope >> lookupVarForDeclaration: name [
	"This is a version of #lookupVar: that skips the OCRequestorScope.
	When looking temp var declarations, we do not want the Requestor scope to automatically
	create that variable. Subclasses override if they do not skip but do a lookup"

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

{ #category : #levels }
OCAbstractScope >> scopeLevel [

	outerScope ifNil: [^ 0].
	^ outerScope scopeLevel + 1
]
