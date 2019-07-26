"
A tree visualization using Athens for display
"
Class {
	#name : #AthensTreeView,
	#superclass : #Object,
	#instVars : [
		'nodeExtentBlock',
		'nodeChildsBlock',
		'root',
		'rows'
	],
	#category : #'Athens-Examples-Demos'
}

{ #category : #examples }
AthensTreeView class >> example1 [
	AthensTreeView
		openOn: Collection
		extentBlock: [ :each | (5 + each instVarNames size) @ (5 + each methodDict size) ]
		childsBlock: [ :el | el subclasses ]
]

{ #category : #'instance creation' }
AthensTreeView class >> openOn: root extentBlock: extBlock childsBlock: childsBlock [

	| scene |
	
	scene := self new root: root extentBlock: extBlock childsBlock: childsBlock.
		

	^ AthensSceneView new
		scene: scene;
		openInWindow.
]

{ #category : #private }
AthensTreeView >> buildSubtreeFor: aNodeObject level: aLevel [
	| node childs |
	
	node := AthensSimpleTreeNode new.
	node 
		subject: aNodeObject;
		extent: (nodeExtentBlock value: aNodeObject).
		
	childs := (nodeChildsBlock value: aNodeObject) collect:  [ :each | self buildSubtreeFor: each level: aLevel+1 ].
	node children: childs.

	(self rowAt: aLevel) add: node.
	
	^ node	

]

{ #category : #private }
AthensTreeView >> layOutGeometry [

	"first do a vertical layout"
	
	| origin |
	
	origin := 0.
	
	
	rows do: [:row |  | rowHeight color |
		
		rowHeight := 0.
		color := Color random.
		
		row do: [ :node |
			node originY: origin.
			node color: color.
			rowHeight := rowHeight max: node height.
		].
	
		origin := origin + rowHeight + self spacingBetweenRows.
	].

	"now do a horizontal one"
	
	root layoutHorizontally: 0.
]

{ #category : #rendering }
AthensTreeView >> renderOn: aCanvas [
	aCanvas surface clear: Color white.
	root renderOn: aCanvas
]

{ #category : #accessing }
AthensTreeView >> root: aRootObject [

	root := self buildSubtreeFor: aRootObject level: 1.
	
	self layOutGeometry
]

{ #category : #'private - initialization' }
AthensTreeView >> root: aRootObject extentBlock: extBlock childsBlock: childsBlock [

	nodeExtentBlock := extBlock.
	nodeChildsBlock := childsBlock.

	root := self buildSubtreeFor: aRootObject level: 1.
	
	self layOutGeometry
]

{ #category : #accessing }
AthensTreeView >> rowAt: aLevel [
	rows ifNil: [  rows := OrderedCollection new ].
	
	[ aLevel > rows size ] whileTrue: [  rows add: OrderedCollection new ].

	^ rows at: aLevel
]

{ #category : #accessing }
AthensTreeView >> spacingBetweenRows [
	^ 30
]
