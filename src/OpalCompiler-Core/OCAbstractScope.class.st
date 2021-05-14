"
I am a symbol table where variable names are associated with SemVars.  Each context (method/closure) get a fresh scope that inherits from its outer scope.
"
Class {
	#name : #OCAbstractScope,
	#superclass : #Object,
	#instVars : [
		'outerScope'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #lookup }
OCAbstractScope >> hasBindingThatBeginsWith: aString [
	"check weather there are any variables defined that start with aString"
	^ outerScope
		ifNil: [ false ]
		ifNotNil: [ outerScope hasBindingThatBeginsWith: aString ]
]

{ #category : #testing }
OCAbstractScope >> hasTempVector [
	^ false
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
OCAbstractScope >> isMethodScope [

	^false
]

{ #category : #lookup }
OCAbstractScope >> lookupVar: name [
	"search the scope (and the outer scopes) for a variable 'name' and return it"
	^ self lookupVar: name declare: true
]

{ #category : #lookup }
OCAbstractScope >> lookupVar: name declare: aBoolean [
	"search the scope (and the outer scopes) for a variable 'name' and return it"
	^self subclassResponsibility
]

{ #category : #lookup }
OCAbstractScope >> lookupVarForDeclaration: name [
	"This is a version of #lookupVar: that skips the OCRequestorScope.
	When looking temp var declarations, we do not want the Requestor scope to automatically
	create that variable. Subclasses override if they do not skip but do a lookup"

	^ self lookupVar: name declare: false
]

{ #category : #lookup }
OCAbstractScope >> outerScope [

	^ outerScope
]

{ #category : #initializing }
OCAbstractScope >> outerScope: aSemScope [

	outerScope := aSemScope
]
