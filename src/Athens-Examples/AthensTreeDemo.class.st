"
A tree visualization using Athens for display
"
Class {
	#name : #AthensTreeDemo,
	#superclass : #Object,
	#instVars : [
		'nodeExtentBlock',
		'nodeChildsBlock',
		'root',
		'rows'
	],
	#category : #'Athens-Examples-Morphic'
}

{ #category : #examples }
AthensTreeDemo class >> example1 [
	<script>
	self
		openOn: Collection
		extentBlock: [ :each | (5 + each slots size) @ (5 + each methods size) ]
		childsBlock: [ :el | el subclasses ]
]

{ #category : #'instance creation' }
AthensTreeDemo class >> openOn: root extentBlock: extBlock childsBlock: childsBlock [

	| scene |

	scene := self new root: root extentBlock: extBlock childsBlock: childsBlock.

	^ AthensSceneView new
		scene: scene;
		openInWindow
]

{ #category : #private }
AthensTreeDemo >> buildSubtreeFor: aNodeObject level: aLevel [
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
AthensTreeDemo >> layOutGeometry [

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

	root layoutHorizontally: 0
]

{ #category : #rendering }
AthensTreeDemo >> renderOn: aCanvas [
	aCanvas surface clear: Color white.
	root renderOn: aCanvas
]

{ #category : #accessing }
AthensTreeDemo >> root: aRootObject [

	root := self buildSubtreeFor: aRootObject level: 1.

	self layOutGeometry
]

{ #category : #'private - initialization' }
AthensTreeDemo >> root: aRootObject extentBlock: extBlock childsBlock: childsBlock [

	nodeExtentBlock := extBlock.
	nodeChildsBlock := childsBlock.

	root := self buildSubtreeFor: aRootObject level: 1.

	self layOutGeometry
]

{ #category : #accessing }
AthensTreeDemo >> rowAt: aLevel [
	rows ifNil: [  rows := OrderedCollection new ].

	[ aLevel > rows size ] whileTrue: [  rows add: OrderedCollection new ].

	^ rows at: aLevel
]

{ #category : #accessing }
AthensTreeDemo >> spacingBetweenRows [
	^ 30
]
