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

{ #category : 'lookup' }
OCAbstractScope >> hasBindingThatBeginsWith: aString [
	"check weather there are any variables defined that start with aString"
	^ outerScope
		ifNil: [ false ]
		ifNotNil: [ outerScope hasBindingThatBeginsWith: aString ]
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
