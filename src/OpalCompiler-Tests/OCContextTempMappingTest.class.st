"
This testcase tests that we can correctly get the value of the temp vars from a context. That is,

 - it finds the correct context where the variable is defined to get the value
 - it handles well copying and temp vectors

Probably tests are missing covering all possible combinations of 
  - temps
  - copying temps
  - temp vectors
  - nested blocks
  - blocks from dead contexts
  - optimized blocks

However this class is a starting point for this.
"
Class {
	#name : #OCContextTempMappingTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #tests }
OCContextTempMappingTest >> fetchArgFromOptimizedBlockInsideFullBlock: anArg extraArg: anotherArg [
	
	| result |
	self class withAllSubclassesDo: [ :each | 
		result := anArg = anArg ifTrue: [ anotherArg value. thisContext tempNamed: #anArg ]].
	^result
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingArgOfOuterBlockFromAnotherDeepBlock [
	
	| actual |
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].
	
	actual := [:outerArg | 
		outerArg asString.
		[ :innerArg | innerArg asString. thisContext tempNamed: #outerArg ] value: #innerValue.
		] value: #outerValue.
	
	self assert: actual equals: #outerValue
]

{ #category : #'test - block arguments' }
OCContextTempMappingTest >> testAccessingBlockArgumentNoneOptimizedBlock [
	" the #in: call is not optimized, the block argument 'x' is a real block local temporary (argument).
(OCContextTempMappingTest>>#testAccessingBlockArgumentNoneOptimizedBlock) numTemps -> 2"

	| a b |

	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	a := 1.
	b := a in: [ :x | thisContext tempNames ].
	self assert: b equals:#(a b x)
]

{ #category : #'test - block arguments' }
OCContextTempMappingTest >> testAccessingBlockArgumentOptimizedBlock [
	" the #ifNotNil: call is an optimized block, the block argument 'x' actually a method temporary.
(OCContextTempMappingTest>>#testAccessingBlockArgumentOptimizedBlock) numTemps -> 3"

	| a b |
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	a := 1.
	b := a ifNotNil: [ :x | thisContext tempNames ].
	self assert: b equals: #(#a #b #x)
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingMethodArgFromOptimizedBlockContext [
	
	| actual |
	actual := self fetchArgFromOptimizedBlockInsideFullBlock: #argValue extraArg: #anotherArg.
	self assert: actual equals: #argValue
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingMultipleVariablesInVector [
	| t1 t2 t3 t4 t5 t6 |
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	t1 := 1.
	t2 := 2.
	t3 := 3.
	t4 := 4.
	t5 := 5.
	t6 := 6.
	[ t5 := 50.
	t6 := 60.
	t3 + t4 ] value.

	self assert: t1 identicalTo: (thisContext tempNamed: 't1').
	self assert: t2 identicalTo: (thisContext tempNamed: 't2').
	self assert: t3 identicalTo: (thisContext tempNamed: 't3').
	self assert: t4 identicalTo: (thisContext tempNamed: 't4').
	self assert: t5 identicalTo: (thisContext tempNamed: 't5').
	self assert: t6 identicalTo: (thisContext tempNamed: 't6')
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingTempsVectorInBlock [
	"Check the source code availability to do not fail on images without sources"

	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	[ | temp |
	[ temp := 1.
	self assert: (thisContext tempNamed: 'temp') identicalTo: 1 ] value ] value
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingTempsVectorInBlock2 [
	| a b r |
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	a := [ r := 'failure' ].
	[ b := 'success'.
	r := thisContext ]
		on: Error
		do: a.

	self assert: r identicalTo: (r tempNamed: 'r').
	self assert: (r tempNamed: 'a') identicalTo: a.
	self assert: (r tempNamed: 'b') identicalTo: b
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingTempsVectorInBlock3 [

	|a b r |

	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	a := [ r := 'failure'].
	[ b :='success'.
	self assert: (thisContext tempNamed: 'a') equals: a. 
	] on: Error do: a.
	self assert: (thisContext tempNamed: 'r') equals: nil.
	self assert: (thisContext tempNamed: 'b') equals: 'success'.
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingVariablesInBlock [
	| theContext |
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	[ | t1 |
	t1 := true.
	theContext := thisContext.
	self assert: (thisContext tempNamed: 'theContext') identicalTo: thisContext.
	self assert: (theContext tempNamed: 't1') ] value
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingVariablesInOptimizedBlock [
	"Check the source code availability to do not fail on images without sources"

	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	1 to: 2 do: [ :index | 
		| optimizedTemp |
		optimizedTemp := index.
		self assert: (thisContext tempNamed: 'optimizedTemp') identicalTo: index ]
]

{ #category : #tests }
OCContextTempMappingTest >> testAccessingVariablesInOptimizedBlock2 [
	| a b |
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifFalse: [ ^ self skip ].

	a := 1.
	b := 2.

	a < b
		ifTrue: [ | c |
			c := 50.
			self assert: (thisContext tempNamed: 'a') identicalTo: 1.
			self assert: (thisContext tempNamed: 'b') identicalTo: 2.
			self assert: (thisContext tempNamed: 'c') identicalTo: 50 ]
]
