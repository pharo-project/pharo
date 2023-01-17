"
I support the same protocol than AthensTransform,
but in addition I store a plain 2x3 matrix with state accessible at any moment.

This is different to AthensTransform because it does not expose its internal storage, because it can be backend specific.
"
Class {
	#name : #AthensAffineTransform,
	#superclass : #AthensTransform,
	#instVars : [
		'sx',
		'shx',
		'x',
		'shy',
		'sy',
		'y'
	],
	#category : #'Athens-Core-Matrices'
}

{ #category : #transformations }
AthensAffineTransform >> clearTranslation [
	"Reset a translation from matrix, leaving only scale and rotation"

	x := y := 0.0
]

{ #category : #initialization }
AthensAffineTransform >> initialize [
	self loadIdentity
]

{ #category : #'vector - transform' }
AthensAffineTransform >> inverseTransform: aPoint [
	"We assume matrix is never degenerated"
	| px py y0 x0 |

	px := aPoint x.
	py := aPoint y.
	"for rotations around +/- half pi"
	(sx = 0 and:[ sy = 0]) ifTrue:[
		^ self inverseTransformHalfPi: aPoint.
		].
	"for rotations around +/- pi or 0"
	(shx = 0 and:[ shy = 0]) ifTrue:[
		^ self inverseTransformPiOrZero: aPoint.
		].
    "otherwise:
     Given straight transformation formulae:

	px := x0*sx + y0*shx + x.
	py := x0*shy + y0*sy + y.

	we doing inverse one, trying to find x0,y0 for rest of given variables (px,py,x,y,sx,sy,shx,shy).

	x0 := px - x - (shx*y0) / sx.
	y0 := py - y - (shy*x0) / sy.

	y0 := py - y - (shy*(px - x - (shx*y0) / sx)) / sy.

	sy * y0  == py - y - (shy*(px - x - (shx*y0) / sx)).

	sx * sy * y0  == (sx*(py - y)) - (shy*(px - x - (shx*y0))).

	sx * sy * y0  == sx*py - (sx*y) - (shy*px) + (shy*x) + (shy*shx*y0).

	(sx * sy * y0) - (shy*shx*y0)  == sx*py - (sx*y) - (shy*px) + (shy*x) .

	y0* ((sx * sy) - (shy*shx))  == sx*py - (sx*y) - (shy*px) + (shy*x) .

"
	y0  := sx*py - (sx*y) - (shy*px) + (shy*x) / ((sx * sy) - (shy*shx)).
	x0 := px - x - (shx*y0) / sx.

	^ x0@y0
]

{ #category : #private }
AthensAffineTransform >> inverseTransformHalfPi: aPoint [
	"Assume sx,sy = 0

	px := y0*shx + x.
	py := x0*shy + y.

	we doing inverse one, trying to find x0,y0 for rest of given variables (px,py,x,y,sx,sy,shx,shy)"
	| px py y0 x0 |

	px := aPoint x.
	py := aPoint y.

	x0 := py - y  / shy.
	y0 := px - x  / shx.

	^ x0@y0
]

{ #category : #private }
AthensAffineTransform >> inverseTransformPiOrZero: aPoint [
	"Assume shx,shy = 0

	px := x0*sx + x.
	py := y0*sy + y.
	we doing inverse one, trying to find x0,y0 for rest of given variables (px,py,x,y,sx,sy,shx,shy)"

	| px py y0 x0 |

	px := aPoint x.
	py := aPoint y.

	x0 := px - x / sx.
	y0 := py - y / sy.

	^ x0@y0
]

{ #category : #transformations }
AthensAffineTransform >> invert [
	"
	Affine matrix can be seen as TM X -> (TM)^-1 = M^1 T^-1
	"

	"Invert the 2x2 part"
	| det isx ishx ishy isy nx ny ix iy |
	det := (sx*sy) - (shy*shx).
	self assert: (det closeTo: 0) not.

	"Prefer direct division to improve floating point precision".
	isx := sy / det. ishx := shx negated / det.
	ishy := shy negated / det. isy := sx / det.
	nx := x negated.
	ny := y negated.

	ix := (isx*nx) + (ishx*ny).
	iy := (ishy*nx) + (isy*ny).

	"Set the result."
	sx := isx. shx := ishx. x := ix.
	shy := ishy. sy := isy. y := iy
]

{ #category : #accessing }
AthensAffineTransform >> inverted [
	"Answer an inverse transformation of receiver"
	| det |
	det := sx * sy - (shx * shy).
	^ self class new
		sx: sy / det;
		sy: sy / det;
		shx: shx * -1 / det;
		shy: shy * -1 / det;
		x: shx * y - (x * sy) / det;
		y: shy * x - (sx * y) / det;
		yourself
]

{ #category : #testing }
AthensAffineTransform >> isAffineTransform [
	^ true
]

{ #category : #testing }
AthensAffineTransform >> isIdentity [
	^ sx = 1 and: [ shx = 0 and: [ x = 0 and: [
	shy = 0 and: [ sy = 1 and: [ y = 0]]]]]
]

{ #category : #transformations }
AthensAffineTransform >> loadAffineTransform: m [

	x := m x.
	y := m y.
	sx := m sx.
	sy := m sy.
	shx := m shx.
	shy := m shy
]

{ #category : #transformations }
AthensAffineTransform >> loadIdentity [
	"Initialize with identity transform"
	sx := sy := 1.0.
	shx := shy := x := y := 0.0
]

{ #category : #transformations }
AthensAffineTransform >> multiplyBy: m [
	"Multiply receiver by given affine matrix"
"
| sx   shx   x   |        | sx'   shx'   x'   |
| shy  sy    y   |   *    | shy'  sy'    y'   |
|  0   0     1   |        | 0  	0   	 1	   |

"

	| nsx nshx nx nshy nsy ny |

	nsx := sx * m sx + (shx  * m shy).
	nshx := sx * m shx + (shx * m sy).
	nx := sx * m x + (shx * m y) + x.

	nshy := shy * m sx + (sy * m shy).
	nsy := shy * m shx  + (sy * m sy).
	ny := shy* m x + (sy * m y) + y.

	sx := nsx.
	sy := nsy.
	shx := nshx.
	shy := nshy.
	x := nx.
	y := ny
]

{ #category : #transformations }
AthensAffineTransform >> restoreAfter: aBlock [
	|previous|
	previous := self copy.
	aBlock ensure: [ 	self loadAffineTransform: previous ]
]

{ #category : #transformations }
AthensAffineTransform >> rotateByDegrees: angle [

	^ self rotateByRadians: angle degreesToRadians
]

{ #category : #transformations }
AthensAffineTransform >> rotateByRadians: angle [

	"Multiply receiver by rotation matrix

| sx   shx   x   |       | cos -sin  0  |        | (sx*cos)+(shx*sin)   (-sx*sin+shx*cos)  x |
| shy   sy   y   |   *   | sin   cos 0  |   ===> | (shy*cos)+(sy*sin)   (-shy*sin)+sy*cos) y |
|  0     0   1   |       | 0     0   1  |        |        0                   0            1 |

"
	| cos sin newSx newSy |

	cos := angle cos.
	sin := angle sin.


	newSx := sx*cos + (shx*sin).
	newSy := sy*cos - (shy*sin).

	shx := shx*cos - (sx*sin).
	shy := shy*cos + (sy*sin).

	sx := newSx.
	sy := newSy
]

{ #category : #accessing }
AthensAffineTransform >> scale [
	^ sx@sy
]

{ #category : #transformations }
AthensAffineTransform >> scaleBy: factor [

	"multiply receiver by uniform scale matrix

| sx   shx   x   |       | (f x) 0 0  |             | sx*(f x)    shx*(f y)  x |
| shy sy     y   |   *   | 0 (f y) 0  |    ===> | shy*(f x)     sy*(f y)    y |
|  0     0     1  |        | 0 0     1  |                |  0      0              1  |

"
	factor isPoint
		ifTrue: [
			sx := sx*factor x.
			shx := shx*factor y.
			sy := sy*factor y.
			shy := shy*factor x ]
		ifFalse: [
			sx := sx*factor.
			shx := shx*factor.
			sy := sy*factor.
			shy := shy*factor ]
]

{ #category : #transformations }
AthensAffineTransform >> scaleX: fx Y: fy [

	"multiply receiver by scale matrix

| sx   shx   x   |       | fx 0 0  |         | sx*fx   shx*fy  x |
| shy   sy   y   |   *   | 0 fy 0  |    ===> | shy*fx  sy*fy   y |
|  0    0    1   |       | 0 0  1  |         |  0        0     1 |

"
	sx := sx*fx.
	shx := shx*fy.
	sy := sy*fy.
	shy := shy*fx
]

{ #category : #accessing }
AthensAffineTransform >> shx [
	^ shx
]

{ #category : #accessing }
AthensAffineTransform >> shx: number [
	shx := number
]

{ #category : #accessing }
AthensAffineTransform >> shy [
	^ shy
]

{ #category : #accessing }
AthensAffineTransform >> shy: number [
	shy := number
]

{ #category : #accessing }
AthensAffineTransform >> sx [
	^ sx
]

{ #category : #accessing }
AthensAffineTransform >> sx: number [
	sx := number
]

{ #category : #accessing }
AthensAffineTransform >> sy [
	^ sy
]

{ #category : #accessing }
AthensAffineTransform >> sy: number [
	sy := number
]

{ #category : #'vector - transform' }
AthensAffineTransform >> transform: aPoint [
	| px py |

	px := aPoint x.
	py := aPoint y.
	^ Point
		x: (sx*px +(shx*py) + x)
		y: (shy*px + (sy*py) + y)
]

{ #category : #'vector - transform' }
AthensAffineTransform >> transformX: px Y: py [
	"Transform x and y coordinates by receiver. Answer a Point"
	^ Point
		x: (sx*px +(shx*py) + x)
		y: (shy*px + (sy*py) + y)
]

{ #category : #transformations }
AthensAffineTransform >> translateBy: aPoint [

	^ self translateX: aPoint x Y: aPoint y
]

{ #category : #transformations }
AthensAffineTransform >> translateX: px Y: py [
	"Multiply receiver by translation matrix :

| sx    shx   x   |       | 1  0  px |        |	sx  shx  (sx*px + shx*py + x)  |
| shy   sy    y   |   *   | 0  1  py |   ===> | shy   sy  (shy*px + sy*py + y)  |
|  0    0     1   |       | 0  0  1  |        |  0     0    1                   |

"
	x := (sx*px) + (shx*py) + x.
	y := (shy*px) + (sy*py) + y
]

{ #category : #transformations }
AthensAffineTransform >> translateX: px y: py [
	"Multiply receiver by translation matrix :

| sx   shx     x   |       | 1  0  px |        |	sx  shx  (sx*px + shx*py + x)  |
| shy   sy     y   |   *   | 0  1  py |   ===> | shy  sy  (shy*px + sy*py + y)  |
|  0     0     1   |       | 0  0   1 |        |  0     0    1                  |

"
	x := (sx*px) + (shx*py) + x.
	y := (shy*px) + (sy*py) + y
]

{ #category : #accessing }
AthensAffineTransform >> translation [
	^ x@y
]

{ #category : #accessing }
AthensAffineTransform >> translation: aPoint [
	x := aPoint x.
	y := aPoint y
]

{ #category : #transformations }
AthensAffineTransform >> transposed [

	| s |
	s := shx.
	shx := shy.
	shy := s
]

{ #category : #accessing }
AthensAffineTransform >> x [
	^ x
]

{ #category : #accessing }
AthensAffineTransform >> x: number [
	x := number
]

{ #category : #accessing }
AthensAffineTransform >> y [
	^ y
]

{ #category : #accessing }
AthensAffineTransform >> y: number [
	y := number
]
