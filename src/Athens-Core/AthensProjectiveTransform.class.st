"
I am an extended version of my supeclass, that represents 2D projective transforms in homogeneous coordinates by using a 3x3 matrix.

<Currently I have no use>
"
Class {
	#name : #AthensProjectiveTransform,
	#superclass : #AthensAffineTransform,
	#instVars : [
		'w0',
		'w1',
		'w2'
	],
	#category : #'Athens-Core-Matrices'
}

{ #category : #testing }
AthensProjectiveTransform >> invert [
	self isAffineTransform ifTrue: [ ^ super invert ].
	self shouldBeImplemented
]

{ #category : #testing }
AthensProjectiveTransform >> isAffineTransform [
	^ w0 = 0 and: [ w1 = 0 and: [ w2 = 1 ] ]
]

{ #category : #transformations }
AthensProjectiveTransform >> loadIdentity [
	"initialize with identity transform"
	super loadIdentity.
	w0 := w1 := 0.0.
	w2 := 1.0
]

{ #category : #transformations }
AthensProjectiveTransform >> transform: aPoint [
	| px py tx ty tw |

	px := aPoint x.
	py := aPoint y.
	"Transform x and y coordinates by receiver, and perform perspective division to answer a 2D point."
	tx := (sx*px +(shx*py) + x).
	ty := (shy*px + (sy*py) + y).
	tw := (w0*px) + (w1*py) + w2.
	^ Point x: tx / tw y: ty / tw
]

{ #category : #transformations }
AthensProjectiveTransform >> transformX: px Y: py [
	"Transform x and y coordinates by receiver, and perform perspective division to answer a 2D point."
	| tx ty tw |
	tx := (sx*px +(shx*py) + x).
	ty := (shy*px + (sy*py) + y).
	tw := (w0*px) + (w1*py) + w2.
	^ Point x: tx / tw y: ty / tw
]

{ #category : #accessing }
AthensProjectiveTransform >> w0 [
	^ w0
]

{ #category : #accessing }
AthensProjectiveTransform >> w0: anObject [
	w0 := anObject
]

{ #category : #accessing }
AthensProjectiveTransform >> w1 [
	^ w1
]

{ #category : #accessing }
AthensProjectiveTransform >> w1: anObject [
	^ w1 := anObject
]

{ #category : #accessing }
AthensProjectiveTransform >> w2 [
	^ w2
]

{ #category : #accessing }
AthensProjectiveTransform >> w2: anObject [
	w2 := anObject
]
