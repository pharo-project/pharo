"
I modelt the scope of a block
"
Class {
	#name : #OCBlockScope,
	#superclass : #OCAbstractMethodScope,
	#category : #OpalCompiler-Core-Semantics
}

{ #category : #testing }
OCBlockScope >> hasEscapingVars [
	^ (copiedVars isEmpty and: [tempVector isEmpty]) not
]

{ #category : #accessing }
OCBlockScope >> inComingCopiedVars [
	^ copiedVars select: [:each | outerScope copiedVars includes: each].
]

{ #category : #testing }
OCBlockScope >> isBlockScope [
	^ true
]

{ #category : #testing }
OCBlockScope >> isInsideOptimizedLoop [
	^ self outerScope isInsideOptimizedLoop
]

{ #category : #lookup }
OCBlockScope >> nextLexicalContextOf: aContext [

	"Returns the next context to lookup a variable name from within this Scope.
	A method should continue looking up a variable from the outerContext.
	
	A BlockClosure's next lexical context is the context where it was created from, and it can access variables from it."
	^ aContext home
]

{ #category : #lookup }
OCBlockScope >> nextOuterScopeContextOf: aContext [

	"Returns the next context to lookup a variable name from within outer scope.
	If it is block context then we return outer context for lookup.
	But if it is method context lookup will continue in same context but within outer scope"
	^ aContext outerContext ifNil: [ aContext ]
]

{ #category : #lookup }
OCBlockScope >> outerScopeLookupVar: name inContext: aContext [
	^ self outerScope 
		lookupVar: name
		inContext: (self nextOuterScopeContextOf: aContext)
]
