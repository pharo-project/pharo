"
A GLMBrickLayouter is Brick's brand new layout system.
It builds a graph where each Brick corresponds two nodes: WidthNode and HeightNode.  Each node can have 4 states:

1 - depends on parent
2 - depends on children
3 - depends on itself
4 - depends on opposite node.

Layouter traverses graph from root WidthNode. Depending on state it behaves differently:

1) If layouter meets node with state 1 it checks if its parent already positioned and its height or width calculated. Otherwise it returns to such node later as soon as it finished with parent. If node with state 1 has parent with state 2 a LayoutCycleException must be reaised.

2) If layouter meets node with state 2 it must skip it and continues traversing all children until all of them are not positioned. As soon as it happens it returns back to skipped node. In case any of children of node with state 2 has state 1 a LayoutCycleException must be reaised.

3) If layouter meets node with state 3 it immidiately calculates corresponding value and applies it. Then it checks if there is an unvisited child node. If yes moves to it, otherwise returns to parent node.

4) If layouter meets node with state 4 it swaps its working node to opposite and continues traversing. For WidthNode opposing node is HeightNode and for HeightNode it is WidthNode.  If both nodes have state 4 a LayoutCycleException must be raised.

Instance Variables
	rootHeightNode:		<Object>
	rootWidthNode:		<Object>

rootHeightNode
	- xxxxx

rootWidthNode
	- xxxxx

"
Class {
	#name : #GLMBrickLayouter,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Glamour-Morphic-Brick-Layouts-Utils'
}

{ #category : #examples }
GLMBrickLayouter class >> example [

	|bricks|
	
	bricks := OrderedCollection new.
	1 to: 13 do: [ :i | bricks add: GLMBrick new ].
	
	(bricks at: 1) 
		width: 100;
		height: 100.
		
	(bricks at: 2)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 3)
		hResizing: #spaceFill;
		vResizing: #opposite.
		
	(bricks at: 4)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
		
	(bricks at: 1)
		addBrickBack: (bricks at: 2);
		addBrickBack: (bricks at: 3);
		addBrickBack: (bricks at: 4).
		
	(bricks at: 5)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
	
	(bricks at: 6)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	
	(bricks at: 7)
		width: 50;
		vResizing: #opposite.
		
	(bricks at: 8)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
		
	(bricks at: 9)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	
	(bricks at: 10)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
		
	(bricks at: 11)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 12)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 13)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 2)
		addBrickBack: (bricks at: 5);
		addBrickBack: (bricks at: 6).
		
	(bricks at: 3)
		addBrickBack: (bricks at: 7);
		addBrickBack: (bricks at: 8).
	
	(bricks at: 4)
		addBrickBack: (bricks at: 9);
		addBrickBack: (bricks at: 10).
		
	(bricks at: 5)
		addBrickBack: (bricks at: 11).
		
	(bricks at: 8)
		addBrickBack: (bricks at: 12);
		addBrickBack: (bricks at: 13).
	"self halt."
	^ bricks
	
	
]

{ #category : #accessing }
GLMBrickLayouter class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : #accessing }
GLMBrickLayouter >> heightTraverser [

	^ GLMBrickHeightTraverser uniqueInstance
]

{ #category : #private }
GLMBrickLayouter >> layout: aBrick [

	aBrick isWidthDirty
		ifTrue: [ self widthTraverser layoutAround: aBrick inContext: nil ].
	aBrick isHeightDirty
		ifTrue: [ self heightTraverser layoutAround: aBrick inContext: nil ]
]

{ #category : #layouting }
GLMBrickLayouter >> layoutIn: aBrick [

	self
		markDependent: aBrick;
		layout: aBrick
]

{ #category : #private }
GLMBrickLayouter >> markDependent: aBrick [

	(aBrick isWidthDirty | aBrick isHeightDirty)
		ifTrue: [
			"we mark layout as dirty of brick itself"
			aBrick layout markDirty.
			"and its direct parent"
			aBrick owner ifNotNil: [ :o | o isBrick ifTrue: [ o layout markDirty ] ] ].

	aBrick isWidthDirty
		ifTrue: [ self widthTraverser markDirtyAround: aBrick inContext: nil ].
	aBrick isHeightDirty
		ifTrue: [ self heightTraverser markDirtyAround: aBrick inContext: nil ]
	
]

{ #category : #accessing }
GLMBrickLayouter >> widthTraverser [

	^ GLMBrickWidthTraverser uniqueInstance
]
