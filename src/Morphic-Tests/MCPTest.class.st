Class {
	#name : #MCPTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests-Kernel'
}

{ #category : #constants }
MCPTest >> defaultBounds [
	"the default bounds for morphs"
	^ 0 @ 0 corner: 50 @ 40 
]

{ #category : #constants }
MCPTest >> defaultTop [
	"the default top for morphs"
	^ self defaultBounds top 
]

{ #category : #tests }
MCPTest >> testTop [
	"test the #top: messages and its consequences"

	| morph factor newTop newBounds |
	morph := Morph new.
	""
	factor := 10.
	newTop := self defaultTop + factor.
	newBounds := self defaultBounds translateBy: 0 @ factor.
	""
	morph top: newTop.
	""
	self
		assert: morph top equals: newTop;
		assert: morph bounds equals: newBounds
]
