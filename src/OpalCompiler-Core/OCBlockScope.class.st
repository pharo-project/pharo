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

	^ outerScope copiedVarNames select: [ :e | self copiedVarNames includes: e ]
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
OCBlockScope >> lookupVar: name [

	"Look up the variable in the current scope and satisfy the clean block rules.
	In particular, we do not care about inlined blocks, since they actually live in their enclosing context"
	
	| found |
	found := super lookupVar: name.
	found ifNil: [ ^ found ].
	
	(self isOptimized or: [ self isClean not ])
		ifTrue: [ ^ found ].
	
	"We are in a non-inlined clean block scope: do not return self, super, ivar, temps nor args"
	(found isSelfVariable or: [ found isSuperVariable ])
		ifTrue: [ ^ nil ].
	
	found isInstanceVariable ifTrue: [ ^ nil ].
	(found isLocalVariable and: [ found scope = self ]) ifFalse: [ ^ nil ].
	
	"The rest of variables (e.g., globals, thisContext) are visible!"
	^ found
]

{ #category : 'lookup' }
OCBlockScope >> nextOuterScopeContextOf: aContext [

	"Returns the next context to lookup a variable name from within outer scope.
	If it is block context then we return outer context for lookup.
	But if it is method context lookup will continue in same context but within outer scope"
	^ aContext outerContext
]
