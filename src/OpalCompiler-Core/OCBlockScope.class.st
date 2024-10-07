"
I modelt the scope of a block
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
OCBlockScope >> nextOuterScopeContextOf: arg1 [

	| tmp1 |
	tmp1 := arg1 outerContext.
	^ tmp1 ifNil: [ arg1 ] ifNotNil: [ tmp1 ]
]
