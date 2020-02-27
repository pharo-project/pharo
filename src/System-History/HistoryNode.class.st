"
A HistoryNode is composite node of an history tree. It is made to contain some other HistoryNode or HistoryLeaf instances.
A subnode is added with #addItem:
In order to add and feed a new subtree, one can use openGroup which add a new HistoryNode. When openGroup is sent to an HistoryNode named H, then a new group G is added and all subsequent sent of #addItem: or of #openGroup to H will update the new node G until G is closed by a closeGroup. 

As examples:
---------------
H := HistoryNode new.
H addItem: (i1 := HistoryLeaf new).
---------------
gives:
H
	i1

---------------
H := HistoryNode new.
H openGroup. ""add a new group named g1""
H addItem: (i1 := HistoryLeaf new).
H addItem: (i2 := HistoryLeaf new).
--------------
gives:
H
	g1
		i1
		i2

--------------
H := HistoryNode new.
H openGroup. ""add a new group named g1""
H openGroup. ""add a new group named g2""
H addItem: (i1 := HistoryLeaf new).
H addItem: (i2 := HistoryLeaf new).
--------------
gives:
H
	g1
		g2
			i1
			i2
			
--------------
H := HistoryNode new.
H openGroup. ""add a new group named g1""
H openGroup. ""add a new group named g2""
H addItem: (i1 := HistoryLeaf new).
H closeGroup. ""close g2""
H addItem: (i2 := HistoryLeaf new).
H closeGroup. ""close g1""
H addItem: (i3 := HistoryLeaf new).
--------------
gives:
H
	g1
		g2
			i1
		i2
	i3
	
Also se HistoryNodeTest.


Instance Variables
	history:		<OrderedCollection>
	opened:		<Boolean>

history
	- The list of subnodes (HistoryLeaf or HistoryNode instances)

opened
	- true if the node is opened for adding

"
Class {
	#name : #HistoryNode,
	#superclass : #HistoryLeaf,
	#instVars : [
		'history',
		'opened'
	],
	#category : #'System-History-Utilities'
}

{ #category : #adding }
HistoryNode >> addItem: anHistoryItem [ 
	self current notNil
		ifTrue: [(self current addItem: anHistoryItem)
				ifTrue: [^ true]].
	self opened
		ifTrue: [self history add: anHistoryItem.
			^ true].
	^ false
]

{ #category : #accessing }
HistoryNode >> at: aPosition [
	^ self history at: aPosition
]

{ #category : #accessing }
HistoryNode >> at: aPosition put: anItem [
	self history at: aPosition put: anItem
]

{ #category : #'opening-closing' }
HistoryNode >> close [
	opened := false
]

{ #category : #'opening-closing' }
HistoryNode >> closeGroup [
	(self current notNil
			and: [self current isComposite
				and: [self current opened]])
		ifTrue: [self current closeGroup]
		ifFalse: [self close]
]

{ #category : #testing }
HistoryNode >> closed [
	^ self opened not
]

{ #category : #copying }
HistoryNode >> copyFrom: start to: stop [
	^ self history copyFrom: start to: stop
]

{ #category : #accessing }
HistoryNode >> current [
	^ self history isEmpty
		ifFalse: [self history last] 
]

{ #category : #accessing }
HistoryNode >> first [
	^ self history first
]

{ #category : #accessing }
HistoryNode >> groupClass [
	^ self class
]

{ #category : #accessing }
HistoryNode >> history [
	^ history ifNil: [history := OrderedCollection new] 
]

{ #category : #testing }
HistoryNode >> isComposite [
	^ true
]

{ #category : #testing }
HistoryNode >> isEmpty [
	^ self history isEmpty
]

{ #category : #accessing }
HistoryNode >> last [
	^ self history last
]

{ #category : #'opening-closing' }
HistoryNode >> open [
	opened := true
]

{ #category : #'opening-closing' }
HistoryNode >> openGroup [
	^ self addItem: self groupClass new
]

{ #category : #testing }
HistoryNode >> opened [
	^ opened ifNil: [opened := true]
]

{ #category : #removing }
HistoryNode >> removeAt: anIndex [
	self history removeAt: anIndex
]

{ #category : #'private ' }
HistoryNode >> removeFirst [
	self history removeFirst
]

{ #category : #removing }
HistoryNode >> removeLast [
	self history removeLast
]

{ #category : #removing }
HistoryNode >> removeLast: count [
	self history removeLast: count
]

{ #category : #initialization }
HistoryNode >> reset [
	history := nil.
	opened := true
]

{ #category : #accessing }
HistoryNode >> size [
	^ self history size
]
