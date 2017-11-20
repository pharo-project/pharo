Class {
	#name : #MethodMapExamples,
	#superclass : #Object,
	#category : #OpalCompiler-Tests-Misc
}

{ #category : #examples }
MethodMapExamples >> defineCopiedVarBecomeDeadContext [
	| a b |
	b := [ a := 1.a := a + 1. (DebuggerMethodMapOpal forMethod: thisContext method)  tempNamed:'a' in:thisContext].
	^ b
]

{ #category : #examples }
MethodMapExamples >> exampleCopiedVarFromDeadContext [
	^ self defineCopiedVarBecomeDeadContext value
]

{ #category : #examples }
MethodMapExamples >> exampleSimpleTemp [
	| b |
	b := 1.
	^(DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedCopying [
	| b |
	b := 1.
	^[ | a |
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedCopying2 [
	| b |
	b := 1.
	^[ | a |
		a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedPutCopying [
	| b |
	b := 1.
	^[ | a |
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext put: 2. thisContext tempNamed: 'b' ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedPutCopying2 [
	| b |
	b := 1.
	^[ | a |
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext put: 2. thisContext outerContext tempNamed: 'b' ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedPutTempVector [
	| b |
	b := 1.
	^[ | a |
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext put: 3. thisContext tempNamed: 'b' ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedPutTempVector2 [
	| b |
	b := 1.
	^[ | a |
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext put: 3. thisContext outerContext tempNamed: 'b' ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempCopyingNestedBlock [
	
	^[| b |
		b := 1.
		[   | a |
		 a := 2.
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempCopyingNestedBlockPROBLEM [
	 | a |
		 a := 2. 
	^[| b | 
		b := 1.
		[  
		 a := b . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempVector [
	| b |
	b := 1.
	^[ | a |
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempVector2 [
	| b |
	b := 1.
	^[ | a |
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext outerContext ] value
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempVectorInOptimizedBlock [
"temp a is in a temp vector and is accessed from with in the optimized ifTrue:ifFalse: block. But 
the definition is not in the outer block, but in the method scope"
	| result a |
	[ a := 1.
	result := true
		ifTrue: [ 1 + ((DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'a' in: thisContext) ]
		ifFalse: [ 4 ] ] value.
	^ result
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempVectorInlinedLoop [
"This test is for accessing temp var c within an inlined loop scope. 
Temp var c is written in the loop and closed in a block -> therefore it is in a tempvector. "
	| a b c |
	c := nil.
	a := 1.
	[ c := 42.
	b := (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed:'c' in: thisContext. a < 1 ]
		whileTrue: [ a := a + 1.
				[ c asString ] ].
	^ b
]

{ #category : #examples }
MethodMapExamples >> exampleTempNamedTempVectorNestedBlock [
	 | a |
      a  := 1.
	^[| b |
		b := a.
		[  
		 b := 2 . (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed: 'b' in: thisContext ] value] value
]
