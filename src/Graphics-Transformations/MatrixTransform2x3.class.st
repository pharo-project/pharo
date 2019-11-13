"
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix containing the transformation from the local coordinate system in the global coordinate system. Thus, transforming points from local to global coordinates is fast and cheap whereas transformations from global to local coordinate systems are relatively expensive.

Implementation Note: It is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here).
"
Class {
	#name : #MatrixTransform2x3,
	#superclass : #DisplayTransform,
	#type : #words,
	#category : #'Graphics-Transformations'
}

{ #category : #'instance creation' }
MatrixTransform2x3 class >> identity [
	^self new setScale: 1.0
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> new [
	^self new: 6
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> newFromStream: s [
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	^ s nextWordsInto: (self new: 6)
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> transformFromLocal: localBounds toGlobal: globalBounds [
	^((self withOffset: (globalBounds center)) composedWithLocal:
		(self withScale: (globalBounds extent / localBounds extent) asFloatPoint))
			composedWithLocal: (self withOffset: localBounds center negated)
"
	^(self identity)
		setScale: (globalBounds extent / localBounds extent) asFloatPoint;
		setOffset: localBounds center negated asFloatPoint;
		composedWithGlobal:(self withOffset: globalBounds center asFloatPoint)
"
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> withAngle: angle [
	^self new setAngle: angle
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> withOffset: aPoint [
	^self identity setOffset: aPoint
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> withRotation: angle [
	^self new setAngle: angle
]

{ #category : #'instance creation' }
MatrixTransform2x3 class >> withScale: aPoint [
	^self new setScale: aPoint
]

{ #category : #comparing }
MatrixTransform2x3 >> = aMatrixTransform2x3 [ 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self class = aMatrixTransform2x3 class ifFalse: [ ^ false ].
	length := self size.
	length = aMatrixTransform2x3 size ifFalse: [ ^ false ].
	1 
		to: self size
		do: [ :i | (self at: i) = (aMatrixTransform2x3 at: i) ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #'element access' }
MatrixTransform2x3 >> a11 [
	^self at: 1
]

{ #category : #'element access' }
MatrixTransform2x3 >> a11: value [
	 self at: 1 put: value
]

{ #category : #'element access' }
MatrixTransform2x3 >> a12 [
	^self at: 2
]

{ #category : #'element access' }
MatrixTransform2x3 >> a12: value [
	 self at: 2 put: value
]

{ #category : #'element access' }
MatrixTransform2x3 >> a13 [
	^self at: 3
]

{ #category : #'element access' }
MatrixTransform2x3 >> a13: value [
	 self at: 3 put: value
]

{ #category : #'element access' }
MatrixTransform2x3 >> a21 [
	 ^self at: 4
]

{ #category : #'element access' }
MatrixTransform2x3 >> a21: value [
	 self at: 4 put: value
]

{ #category : #'element access' }
MatrixTransform2x3 >> a22 [
	 ^self at: 5
]

{ #category : #'element access' }
MatrixTransform2x3 >> a22: value [
	 self at: 5 put: value
]

{ #category : #'element access' }
MatrixTransform2x3 >> a23 [
	 ^self at: 6
]

{ #category : #'element access' }
MatrixTransform2x3 >> a23: value [
	 self at: 6 put: value
]

{ #category : #converting }
MatrixTransform2x3 >> asMatrixTransform2x3 [
	^self
]

{ #category : #accessing }
MatrixTransform2x3 >> at: index [
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : #accessing }
MatrixTransform2x3 >> at: index put: value [
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
]

{ #category : #'objects from disk' }
MatrixTransform2x3 >> byteSize [
	^self basicSize * self bytesPerBasicElement
]

{ #category : #'objects from disk' }
MatrixTransform2x3 >> bytesPerBasicElement [
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^4
]

{ #category : #composing }
MatrixTransform2x3 >> composedWithLocal: aTransformation [
	"Return the composition of the receiver and the local transformation passed in"
	aTransformation isMatrixTransform2x3 ifFalse:[^super composedWithLocal: aTransformation].
	^self composedWithLocal: aTransformation asMatrixTransform2x3 into: self class new
]

{ #category : #composing }
MatrixTransform2x3 >> composedWithLocal: aTransformation into: result [ 
	"Return the composition of the receiver and the local transformation passed in.
	Store the composed matrix into result."
	| a11 a12 a13 a21 a22 a23 b11 b12 b13 b21 b22 b23 matrix |
	<primitive: 'primitiveComposeMatrix' module: 'Matrix2x3Plugin'>
	matrix := aTransformation asMatrixTransform2x3.
	a11 := self a11.
	b11 := matrix a11.
	a12 := self a12.
	b12 := matrix a12.
	a13 := self a13.
	b13 := matrix a13.
	a21 := self a21.
	b21 := matrix a21.
	a22 := self a22.
	b22 := matrix a22.
	a23 := self a23.
	b23 := matrix a23.
	result a11: a11 * b11 + (a12 * b21).
	result a12: a11 * b12 + (a12 * b22).
	result a13: a13 + (a11 * b13) + (a12 * b23).
	result a21: a21 * b11 + (a22 * b21).
	result a22: a21 * b12 + (a22 * b22).
	result a23: a23 + (a21 * b13) + (a22 * b23).
	^ result
]

{ #category : #'transforming rects' }
MatrixTransform2x3 >> globalBounds: srcRect toLocal: dstRect [
	"Transform aRectangle from global coordinates into local coordinates"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>
	^super globalBoundsToLocal: srcRect
]

{ #category : #'transforming rects' }
MatrixTransform2x3 >> globalBoundsToLocal: aRectangle [
	"Transform aRectangle from global coordinates into local coordinates"
	^self globalBounds: aRectangle toLocal: Rectangle new
]

{ #category : #'transforming points' }
MatrixTransform2x3 >> globalPointToLocal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>
	^(self invertPoint: aPoint) rounded
]

{ #category : #comparing }
MatrixTransform2x3 >> hash [
	| result |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 
		to: self size
		do: [ :i | result := result + (self basicAt: i) ].
	^ result bitAnd: 536870911
]

{ #category : #accessing }
MatrixTransform2x3 >> inverseTransformation [
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 := self invertPoint: 0 @ 0.
	r1 := (self invertPoint: 1 @ 0) - r3.
	r2 := (self invertPoint: 0 @ 1) - r3.
	m := self species new.
	m
		a11: r1 x;
		a12: r2 x;
		a13: r3 x;
		a21: r1 y;
		a22: r2 y;
		a23: r3 y.
	^ m
]

{ #category : #'transforming points' }
MatrixTransform2x3 >> invertPoint: aPoint [ 
	"Transform aPoint from global coordinates into local coordinates"
	| x y det a11 a12 a21 a22 detX detY |
	x := aPoint x asFloat - self a13.
	y := aPoint y asFloat - self a23.
	a11 := self a11.
	a12 := self a12.
	a21 := self a21.
	a22 := self a22.
	det := a11 * a22 - (a12 * a21).
	det = 0.0 ifTrue: [ ^ 0 @ 0 ].	"So we have at least a valid result"
	det := 1.0 / det.
	detX := x * a22 - (a12 * y).
	detY := a11 * y - (x * a21).
	^ (detX * det) @ (detY * det)
]

{ #category : #testing }
MatrixTransform2x3 >> isIdentity [
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	<primitive: 'primitiveIsIdentity' module: 'Matrix2x3Plugin'>
	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]
]

{ #category : #testing }
MatrixTransform2x3 >> isMatrixTransform2x3 [
	"Return true if the receiver is 2x3 matrix transformation"
	^true
]

{ #category : #testing }
MatrixTransform2x3 >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."
	<primitive: 'primitiveIsPureTranslation' module: 'Matrix2x3Plugin'>
	^self a11 = 1.0 and:[self a12 = 0.0 and:[self a22 = 0.0 and:[self a21 = 1.0]]]
]

{ #category : #'transforming rects' }
MatrixTransform2x3 >> localBounds: srcRect toGlobal: dstRect [
	"Transform aRectangle from local coordinates into global coordinates"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>
	^super localBoundsToGlobal: srcRect
]

{ #category : #'transforming rects' }
MatrixTransform2x3 >> localBoundsToGlobal: aRectangle [
	"Transform aRectangle from local coordinates into global coordinates"
	^self localBounds: aRectangle toGlobal: Rectangle new
]

{ #category : #'transforming points' }
MatrixTransform2x3 >> localPointToGlobal: aPoint [
	"Transform aPoint from local coordinates into global coordinates"
	<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>
	^(self transformPoint: aPoint) rounded
]

{ #category : #accessing }
MatrixTransform2x3 >> offset [
	^self a13 @ self a23
]

{ #category : #accessing }
MatrixTransform2x3 >> offset: aPoint [
	self a13: aPoint x asFloat.
	self a23: aPoint y asFloat.
]

{ #category : #printing }
MatrixTransform2x3 >> printOn: aStream [
	aStream 
		nextPutAll: self class name;
		nextPut: $(;
		cr; print: self a11; tab; print: self a12; tab; print: self a13;
		cr; print: self a21; tab; print: self a22; tab; print: self a23;
		cr; nextPut:$).
]

{ #category : #'objects from disk' }
MatrixTransform2x3 >> restoreEndianness [
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."
	| w b1 b2 b3 b4 |
	EndianDetector isLittleEndian ifTrue: 
		[ 1 
			to: self basicSize
			do: 
				[ :i | 
				w := self basicAt: i.
				b1 := w byteAt: 1.
				b2 := w byteAt: 2.
				b3 := w byteAt: 3.
				b4 := w byteAt: 4.
				w := (b1 << 24) + (b2 << 16) + (b3 << 8) + b4.
				self 
					basicAt: i
					put: w ] ]
]

{ #category : #private }
MatrixTransform2x3 >> setAngle: angle [
	"Set the raw rotation angle in the receiver"
	| rad s c |
	rad := angle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c.
]

{ #category : #initialize }
MatrixTransform2x3 >> setIdentiy [
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0; a12: 0.0; a13: 0.0;
		a21: 0.0; a22: 1.0; a23: 0.0.
]

{ #category : #private }
MatrixTransform2x3 >> setOffset: aPoint [ 
	"Set the raw offset in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat
]

{ #category : #private }
MatrixTransform2x3 >> setScale: aPoint [ 
	"Set the raw scale in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a11: pt x asFloat.
	self a22: pt y asFloat
]

{ #category : #'transforming points' }
MatrixTransform2x3 >> transformDirection: aPoint [ 
	"Transform aPoint from local coordinates into global coordinates"
	| x y |
	x := aPoint x * self a11 + (aPoint y * self a12).
	y := aPoint x * self a21 + (aPoint y * self a22).
	^ x @ y
]

{ #category : #'transforming points' }
MatrixTransform2x3 >> transformPoint: aPoint [ 
	"Transform aPoint from local coordinates into global coordinates"
	| x y |
	x := aPoint x * self a11 + (aPoint y * self a12) + self a13.
	y := aPoint x * self a21 + (aPoint y * self a22) + self a23.
	^ x @ y
]

{ #category : #'objects from disk' }
MatrixTransform2x3 >> writeOn: aStream [
	aStream nextWordsPutAll: self.
]
