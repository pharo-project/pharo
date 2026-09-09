Class {
	#name : 'OCEvaluatorTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'tests' }
OCEvaluatorTest >> doTestDebuggerTempAccessWith: one with: two [
	"Test debugger access for temps"

	| outerContext local1 remote1 |
	outerContext := thisContext.
	local1 := 3.
	remote1 := 1 / 2.
	self assert: (self evaluate: 'one' in: thisContext to: self) identicalTo: one.
	self assert: (self evaluate: 'two' in: thisContext to: self) identicalTo: two.
	self assert: (self evaluate: 'local1' in: thisContext to: self) identicalTo: local1.
	self assert: (self evaluate: 'remote1' in: thisContext to: self) identicalTo: remote1.
	self evaluate: 'local1 := -3.0' in: thisContext to: self.
	self assert: local1 equals: -3.0.
	(1 to: 2)
		do: [ :i |
			| local2 r1 r2 r3 r4 |
			local2 := i * 3.
			remote1 := local2 / 7.
			self assert: thisContext ~~ outerContext.
			self assert: (r1 := self evaluate: 'one' in: thisContext to: self) identicalTo: one.
			self assert: (r2 := self evaluate: 'two' in: thisContext to: self) identicalTo: two.
			self assert: (r3 := self evaluate: 'i' in: thisContext to: self) identicalTo: i.
			self assert: (r4 := self evaluate: 'local2' in: thisContext to: self) identicalTo: local2.
			self assert: (r4 := self evaluate: 'remote1' in: thisContext to: self) identicalTo: remote1.
			self assert: (r4 := self evaluate: 'remote1' in: outerContext to: self) identicalTo: remote1.
			self evaluate: 'local2 := 15' in: thisContext to: self.
			self assert: local2 equals: 15.
			self evaluate: 'local1 := 25' in: thisContext to: self.
			self assert: local1 equals: 25.
			{r1 . r2 . r3 . r4}	"placate the compiler" ].
	self assert: local1 equals: 25.
	"this is 25 even though the var is a local, non escaping variable that was copied into the block.
	But the DoIt compiles temp acces using #tempAt:put:, which updates the copies and the original"
	self assert: remote1 equals: 6 / 7
]

{ #category : 'running' }
OCEvaluatorTest >> evaluate: aString in: aContext to: anObject [

	^ OpalEvaluator new
		  context: aContext;
		  evaluate: aString
]

{ #category : 'tests' }
OCEvaluatorTest >> testDebuggerTempAccess [

	self doTestDebuggerTempAccessWith: 1 with: 2
]

{ #category : 'tests - empty' }
OCEvaluatorTest >> testEmptyBlockOneArgument [

	self assert: (OpalEvaluator new evaluate: '[ :a ] value: 1') isNil description: 'Empty blocks should return nil'.
	self assert: (OpalEvaluator new evaluate: '[ :a | ] value: 1') isNil description: 'Empty blocks should return nil'.
	self assert: (OpalEvaluator new evaluate: '[ :a | | t | ] value: 1') isNil description: 'Empty blocks should return nil'
]

{ #category : 'tests - empty' }
OCEvaluatorTest >> testEmptyBlockTwoArguments [

	self
		assert: (OpalEvaluator new evaluate: '[ :a :b ] value: 1 value: 2') isNil
		description: 'Empty blocks should return nil'.
	self
		assert: (OpalEvaluator new evaluate: '[ :a :b | ] value: 1 value: 2') isNil
		description: 'Empty blocks should return nil'.
	self
		assert: (OpalEvaluator new evaluate: '[ :a :b | | t | ] value: 1 value: 2') isNil
		description: 'Empty blocks should return nil'
]

{ #category : 'tests - empty' }
OCEvaluatorTest >> testEmptyBlockZeroArguments [

	self assert: (OpalEvaluator new evaluate: '[ ] value') isNil description: 'Empty blocks should return nil'.
	self assert: (OpalEvaluator new evaluate: '[ | t | ] value') isNil description: 'Empty blocks should return nil'
]
