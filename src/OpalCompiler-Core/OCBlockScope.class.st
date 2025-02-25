"
I model the scope of a block in particular if it has escaping variables (variables that can outlives the block execution).
"
Class {
	#name : 'OCBlockScope',
	#superclass : 'OCAbstractMethodScope',
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'testing' }
OCBlockScope >> hasEscapingVars [
	^ self inComingCopiedVarNames isNotEmpty
]

{ #category : 'accessing' }
OCBlockScope >> inComingCopiedVarNames [
	^ self copiedVarNames intersection: outerScope copiedVarNames
]

{ #category : 'testing' }
OCBlockScope >> isBlockScope [
	^ true
]

{ #category : 'testing' }
OCBlockScope >> isInsideOptimizedLoop [
	^ self outerScope isInsideOptimizedLoop
]

{ #category : 'testing' }
OCBlockScope >> isOptimized [

	^ false
]

{ #category : 'lookup' }
OCBlockScope >> nextOuterScopeContextOf: aContext [

	"Returns the next context to lookup a variable name from within outer scope.
	If it is block context then we return outer context for lookup.
	But if it is method context lookup will continue in same context but within outer scope"
	^ aContext outerContext ifNil: [ aContext ]
]
