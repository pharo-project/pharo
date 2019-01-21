"
Instance variable:
- nonActiveBlockContext: the Context of a block whose home context is not in its sender chain (because the method that created this block already returned)
"
Class {
	#name : #ContextTest2,
	#superclass : #TestCase,
	#instVars : [
		'nonActiveBlockContext'
	],
	#classVars : [
		'Block'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #helper }
ContextTest2 class >> createBlock [
	^ [ thisContext ].
]

{ #category : #helper }
ContextTest2 class >> returnNonActiveContextOfBlock [
	Block := self createBlock.
	^ Block value.
]

{ #category : #running }
ContextTest2 >> setUp [
	super setUp.
	nonActiveBlockContext := self class returnNonActiveContextOfBlock.
]

{ #category : #tests }
ContextTest2 >> testActiveBlockContextHomeAndActiveHome [
	[ self assert: thisContext home == thisContext activeHome.] value.


]

{ #category : #tests }
ContextTest2 >> testNonActiveBlockContextActiveHome [
	self assert: (nonActiveBlockContext activeHome) isNil.
]

{ #category : #tests }
ContextTest2 >> testNonActiveBlockContextHome [
	self assert: (nonActiveBlockContext home) isNotNil.
]
