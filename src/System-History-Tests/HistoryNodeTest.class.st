"
SUnit tests for HistoryNode
"
Class {
	#name : #HistoryNodeTest,
	#superclass : #TestCase,
	#category : #'System-History-Tests'
}

{ #category : #tests }
HistoryNodeTest >> testClose [
	| h i |
	h := HistoryNode new.
	h close.
	h addItem: (i := HistoryLeaf new).
	h open.
 	h addItem: i.
	self assert: h current = i
]

{ #category : #tests }
HistoryNodeTest >> testEmptyHistory [
	| h |
	h := HistoryNode new.
	self assert: h size = 0.
	self assert: h current isNil
]

{ #category : #tests }
HistoryNodeTest >> testGroup [
	| h i |
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	self assert: (h at: 1) = i.
	self assert: (h at: 2) isComposite.

]

{ #category : #tests }
HistoryNodeTest >> testOneGroup [
	| h i c i2 i3 i4 | 
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	self assert: (h at: 1) = i.
	self assert: (c := h at: 2) isComposite.
	self assert: c isEmpty.
	h addItem: (i2 := HistoryLeaf new).
	self assert: h size = 2.
	self assert: c size = 1.
	self assert: c current = i2.
	h closeGroup.
	h addItem: (i3 := HistoryLeaf new).
	self assert: h size = 3.
	self assert: (h at: 3) = i3.
	h closeGroup.
	h addItem: (i4 := HistoryLeaf new).
	self assert: h size = 3.
	

	
	

]

{ #category : #tests }
HistoryNodeTest >> testOneItem [
	| h i |
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: h current = i
]

{ #category : #tests }
HistoryNodeTest >> testReset [
	| h i |
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h current = i.
	self assert: h size = 1.
	h reset.
	self assert: h size = 0.
]

{ #category : #tests }
HistoryNodeTest >> testTwoConsecutiveCloseGroup [
	| h i grp1 grp2 i3 | 
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	grp1 := h at: 2.
	h openGroup.
	self assert: h size = 2.
	self assert: grp1 size = 1.
	grp2 := grp1 at: 1.
	self assert: grp2 size = 0.
	h closeGroup.
	h closeGroup.
	h addItem: (i3 := HistoryLeaf new).
	self assert: h size = 3.
	self assert: h current = i3.
	

	
	

]

{ #category : #tests }
HistoryNodeTest >> testTwoGroups [
	| h i grp1 grp2  i2 i3 | 
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	grp1 := h at: 2.
	h openGroup.
	self assert: h size = 2.
	self assert: grp1 size = 1.
	grp2 := grp1 at: 1.
	self assert: grp2 size = 0.
	h addItem: (i2 := HistoryLeaf new).
	self assert: grp2 size = 1.
	h closeGroup.
	h addItem: (i2 := HistoryLeaf new).
	self assert: grp2 size = 1.
	self assert: grp1 size = 2.
	self assert: grp1 current = i2.
	h closeGroup.
	h addItem: (i3 := HistoryLeaf new).
	self assert: grp2 size = 1.
	self assert: grp1 size = 2.
	self assert: h size = 3.
	self assert: h current = i3.
	

	
	

]
