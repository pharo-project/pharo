"
A node in an AthensTreeView
"
Class {
	#name : #AthensSimpleTreeNode,
	#superclass : #Object,
	#instVars : [
		'children',
		'box',
		'subject',
		'extent',
		'layoutWidth',
		'originX',
		'originY',
		'color'
	],
	#category : #'Athens-Examples-Utilities'
}

{ #category : #accessing }
AthensSimpleTreeNode >> box [

	^ box
]

{ #category : #accessing }
AthensSimpleTreeNode >> box: anObject [

	box := anObject
]

{ #category : #accessing }
AthensSimpleTreeNode >> children [
	^ children
]

{ #category : #accessing }
AthensSimpleTreeNode >> children: aCollection [

	children := aCollection
]

{ #category : #accessing }
AthensSimpleTreeNode >> color [
	^ color
]

{ #category : #accessing }
AthensSimpleTreeNode >> color: aColor [
	color := aColor
]

{ #category : #accessing }
AthensSimpleTreeNode >> extent [

	^ extent
]

{ #category : #accessing }
AthensSimpleTreeNode >> extent: anObject [

	extent := anObject
]

{ #category : #accessing }
AthensSimpleTreeNode >> height [

	^ extent y
]

{ #category : #private }
AthensSimpleTreeNode >> layoutHorizontally: center [
	| pos |
	"position ourselves in the middle X"
	originX := center - (self width * 0.5).

	"childs"
	pos := center - (self layoutWidth *0.5).

	children do: [ :each |
		each layoutHorizontally: pos + (each layoutWidth *0.5).
		pos := pos + each layoutWidth + self spacingBetweenChilds.
	]
]

{ #category : #accessing }
AthensSimpleTreeNode >> layoutWidth [

	^ layoutWidth ifNil: [
		| childWidth |

		childWidth := 0.
		children isEmpty ifFalse: [
			children do: [ :each | childWidth := childWidth + each layoutWidth  ].
			childWidth := childWidth + (self spacingBetweenChilds * (children size-1)).
		].

		layoutWidth := self width max: childWidth.
	]
]

{ #category : #accessing }
AthensSimpleTreeNode >> midBottom [
	^ originX +(extent x *0.5) @ (originY + extent y)
]

{ #category : #accessing }
AthensSimpleTreeNode >> midTop [
	^ originX +(extent x *0.5) @ originY
]

{ #category : #accessing }
AthensSimpleTreeNode >> originX [
	^ originX
]

{ #category : #accessing }
AthensSimpleTreeNode >> originY [
	^ originY
]

{ #category : #accessing }
AthensSimpleTreeNode >> originY: aNumber [
	originY := aNumber
]

{ #category : #rendering }
AthensSimpleTreeNode >> renderOn: aCanvas [

	aCanvas setPaint: color.
	aCanvas drawShape: (originX @ originY extent: extent).
"	children do: [ :each |
		each renderOn: aCanvas
	].
	[  ^ self ] value.
	"
	children do: [ :each | | path |

		"we cache the path connecting each child and its parent, to not recreate it each time"
		path := aCanvas cacheAt: each ifAbsentPut: [
			| midTop midBot |

			midTop := self midBottom.
			midBot := each midTop.

			aCanvas createPath: [:builder |
				builder
					absolute;
					moveTo: self midBottom;
					curveVia: midTop x@midBot y and: midBot x@ midTop y to: midBot
					"lineTo: each midTop"
			]].

		aCanvas setStrokePaint: (Color red alpha:0.5).
		aCanvas drawShape: path.

		each renderOn: aCanvas
	]
]

{ #category : #defaults }
AthensSimpleTreeNode >> spacingBetweenChilds [
	^ 10
]

{ #category : #accessing }
AthensSimpleTreeNode >> subject [

	^ subject
]

{ #category : #accessing }
AthensSimpleTreeNode >> subject: anObject [

	subject := anObject
]

{ #category : #accessing }
AthensSimpleTreeNode >> width [

	^ extent x
]
