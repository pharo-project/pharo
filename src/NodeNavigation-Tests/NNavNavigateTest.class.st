"
I tests the navigations directions
"
Class {
	#name : #NNavNavigateTest,
	#superclass : #TestCase,
	#category : #NodeNavigation-Tests
}

{ #category : #sibling }
NNavNavigateTest >> testNextBrotherItsTheNextOfTheChilds [
	| bigBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'pp'. 
	parent addNode: bigBrother.
	
	littleBrother := RBVariableNode named: 'roque'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionSibling brotherNodeFor: bigBrother .
	self assert: brother equals: littleBrother  .
	

]

{ #category : #sibling-prev }
NNavNavigateTest >> testNextBrotherItsThePreviousOfTheChilds [
	| bigBrother middleBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'big'. 
	parent addNode: bigBrother.
	
	middleBrother := RBVariableNode named: 'middle'. 
	parent addNode: middleBrother.
	
	littleBrother := RBVariableNode named: 'little'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionPreviousBrother brotherNodeFor: middleBrother .
	self assert: brother equals: bigBrother  .
	

]

{ #category : #sibling-prev }
NNavNavigateTest >> testNextBrotherWhenItsTheFirstItsTheLast [
	| bigBrother middleBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'big'. 
	parent addNode: bigBrother.
	
	middleBrother := RBVariableNode named: 'middle'. 
	parent addNode: middleBrother.
	
	littleBrother := RBVariableNode named: 'little'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionPreviousBrother brotherNodeFor: bigBrother.
	self assert: brother equals: littleBrother .
]

{ #category : #sibling }
NNavNavigateTest >> testNextBrotherWhenItsTheLasItsTheFirst [
	| bigBrother littleBrother brother parent |
	
	parent := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'pp'. 
	parent addNode: bigBrother.
	
	littleBrother := RBVariableNode named: 'roque'.
	parent addNode: littleBrother.
	
	brother := NNavDirectionSibling brotherNodeFor: littleBrother.
	self assert: brother equals: bigBrother .
]

{ #category : #children }
NNavNavigateTest >> testNextChildWhenHasChildsIsTheBiggerOne [
	| bigBrother littleBrother child parentRenamed |
	
	parentRenamed := RBSequenceNode new.
	bigBrother := RBVariableNode named: 'pp'. 
	parentRenamed addNode: bigBrother.
	
	littleBrother := RBVariableNode named: 'roque'.
	parentRenamed addNode: littleBrother.
	
	child := NNavDirectionFirstChild childNodeFor:  parentRenamed .
	self assert: child equals: bigBrother .
]

{ #category : #children }
NNavNavigateTest >> testNextChildWhenItHasNotChildsItIsNil [
	| child parent |
	
	parent := RBSequenceNode new.
	
	child := NNavDirectionFirstChild childNodeFor:  parent .
	self assert: child isNil .
]

{ #category : #sibling }
NNavNavigateTest >> testNotBrotherWhenOrphan [
	| orphanNode brother |
	
	orphanNode := RBVariableNode named: 'roque'.
	brother := NNavDirectionPreviousBrother brotherNodeFor: orphanNode.
	self assert: brother isNil.
]

{ #category : #sibling-prev }
NNavNavigateTest >> testNotPreviousBrotherWhenOrphan [
	| orphanNode brother |
	
	orphanNode := RBVariableNode named: 'roque'.
	brother := NNavDirectionPreviousBrother  brotherNodeFor: orphanNode.
	self assert: brother isNil.
]

{ #category : #sibling }
NNavNavigateTest >> testSameNodeIsTheBrotherWhenOnlyChild [
	| uniqueChildNode brother parent |
	
	parent := RBSequenceNode new.
	uniqueChildNode := RBVariableNode named: 'roque'.
	parent addNode: uniqueChildNode.
	
	self assert: uniqueChildNode parent equals: parent.
	self assert: parent children size equals: 1.
	
	brother := NNavDirectionSibling brotherNodeFor: uniqueChildNode.
	self assert: brother equals: uniqueChildNode .
]
