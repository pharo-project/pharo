"
I modelt the scope of a block
"
Class {
	#name : #OCBlockScope,
	#superclass : #OCAbstractMethodScope,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
OCBlockScope >> hasEscapingVars [
	^ self inComingCopiedVarNames isNotEmpty
]

{ #category : #accessing }
OCBlockScope >> inComingCopiedVarNames [

	^ outerScope copiedVarNames select: [ :e | self copiedVarNames includes: e ]
]

{ #category : #testing }
OCBlockScope >> isBlockScope [
	^ true
]

{ #category : #testing }
OCBlockScope >> isInsideOptimizedLoop [
	^ self outerScope isInsideOptimizedLoop
]

{ #category : #testing }
OCBlockScope >> isOptimized [

	^ false
]

{ #category : #lookup }
OCBlockScope >> nextOuterScopeContextOf: aContext [

	"Returns the next context to lookup a variable name from within outer scope.
	If it is block context then we return outer context for lookup.
	But if it is method context lookup will continue in same context but within outer scope"
	^ aContext outerContext ifNil: [ aContext ]
]
