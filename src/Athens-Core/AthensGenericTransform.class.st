"
I am an extended version of my supeclass,
and I represent generic 3x3 matrix.

<Currently I have no use>
"
Class {
	#name : #AthensGenericTransform,
	#superclass : #AthensAffineTransform,
	#instVars : [
		'w0',
		'w1',
		'w2'
	],
	#category : 'Athens-Core-Matrices'
}

{ #category : #transformations }
AthensGenericTransform >> loadIdentity [
	"initialize with identity transform"
	super loadIdentity.
	w0 := w1 := 0.0.
	w2 := 1.0
]
