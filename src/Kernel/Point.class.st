"
I represent an x-y pair of numbers usually designating a location on the screen.

My instances are created either using the message @ or x:y: or r:degrees: as follows:

[[[
| pt |
pt := 10@20.
pt x 
> 10
pt y
> 20 			 
]]]

[[[
| pt |
pt := Point x: 10 y: 20.
pt x 
> 10
pt y
> 20 			 
]]]

I define many nice messages that deal with point such as: 
- arithmetic such as +, *, reciprocal, min, abs,
- comparison <, <=, =, >, >=, closeTo: 
- geometry such as sideOf:, to:intersects:to:, 
- polar coordinates,
- extent such as scaleTo:
- transformation such as negated, translatedBy:, scaleBy:
- rounding with roundTo:, roundUpTo:, truncateTo:, truncated


"
Class {
	#name : #Point,
	#superclass : #Object,
	#instVars : [
		'x',
		'y'
	],
	#category : #'Kernel-BasicObjects'
}

{ #category : #'instance creation' }
Point class >> x: xInteger y: yInteger [ 
	"Answer an instance of me with coordinates xInteger and yInteger."

	^ self basicNew setX: xInteger setY: yInteger
]

{ #category : #arithmetic }
Point >> * arg [ 
	"Answer a Point that is the product of the receiver and arg."

	arg isPoint ifTrue: [^ (x * arg x) @ (y * arg y)].
	^ arg adaptToPoint: self andSend: #*
]

{ #category : #arithmetic }
Point >> + arg [ 
	"Answer a Point that is the sum of the receiver and arg."

	arg isPoint ifTrue: [^ (x + arg x) @ (y + arg y)].
	^ arg adaptToPoint: self andSend: #+
]

{ #category : #arithmetic }
Point >> - arg [ 
	"Answer a Point that is the difference of the receiver and arg."

	arg isPoint ifTrue: [^ (x - arg x) @ (y - arg y)].
	^ arg adaptToPoint: self andSend: #-
]

{ #category : #arithmetic }
Point >> / arg [ 
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x / arg x) @ (y / arg y)].
	^ arg adaptToPoint: self andSend: #/
]

{ #category : #arithmetic }
Point >> // arg [ 
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x // arg x) @ (y // arg y)].
	^ arg adaptToPoint: self andSend: #//
]

{ #category : #comparing }
Point >> < aPoint [ 
	"Answer whether the receiver is above and to the left of aPoint."

	^ x < aPoint x and: [y < aPoint y]
]

{ #category : #comparing }
Point >> <= aPoint [ 
	"Answer whether the receiver is neither below nor to the right of aPoint."

	^ x <= aPoint x and: [y <= aPoint y]
]

{ #category : #comparing }
Point >> = aPoint [

	^ self species = aPoint species
		ifTrue: [ x = aPoint x and: [ y = aPoint y ]]
		ifFalse: [ false ]
]

{ #category : #comparing }
Point >> > aPoint [
	"Answer whether the receiver is below and to the right of aPoint."

	^ x > aPoint x and: [ y > aPoint y ]
]

{ #category : #comparing }
Point >> >= aPoint [
	"Answer whether the receiver is neither above nor to the left of aPoint."

	^ x >= aPoint x and: [ y >= aPoint y ]
]

{ #category : #arithmetic }
Point >> \\ arg [ 
	"Answer a Point that is the mod of the receiver and arg."

	arg isPoint ifTrue: [^ (x \\ arg x) @ (y \\ arg y)].
	^ arg adaptToPoint: self andSend: #\\
]

{ #category : #arithmetic }
Point >> abs [
	"Answer a Point whose x and y are the absolute values of the receiver's x and y."

	^ x abs @ y abs
]

{ #category : #converting }
Point >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a Collection, return a Collection of the results of each element combined with me in that expression."

	^ rcvr collect: [ :element | element perform: selector with: self ]
]

{ #category : #converting }
Point >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Point."
	^ rcvr@rcvr perform: selector with: self
]

{ #category : #transforming }
Point >> adhereTo: aRectangle [
	"If the receiver lies outside aRectangle, return the nearest point on the boundary of the rectangle, otherwise return self."

	(aRectangle containsPoint: self) ifTrue: [^ self].
	^ ((x max: aRectangle left) min: aRectangle right)
		@ ((y max: aRectangle top) min: aRectangle bottom)
]

{ #category : #converting }
Point >> asFloatPoint [
	"Convert me to a float point transforming both of my coordinates
	to floats using #asFloat. If x and y are already represented
	by floats return myself, otherwise create a new Point instance."
	self isFloatPoint
		ifTrue: [ ^ self ].
	^ x asFloat @ y asFloat
]

{ #category : #converting }
Point >> asIntegerPoint [
	"Convert me to an integer point transforming both of my coordinates
	to integers using #asInteger. If x and y are already represented
	by integers return myself, otherwise create a new Point instance."
	self isIntegerPoint
		ifTrue: [ ^ self ].
	^ x asInteger @ y asInteger
]

{ #category : #converting }
Point >> asMargin [
	^ Margin fromPoint: self
]

{ #category : #converting }
Point >> asNonFractionalPoint [
	(x isFraction or: [ y isFraction ])
		ifTrue: [ ^ x asFloat @ y asFloat ]
]

{ #category : #converting }
Point >> asPoint [
	"Answer the receiver itself."
	^self
]

{ #category : #private }
Point >> bitShiftPoint: bits [ 
	x := x bitShift: bits.
	y := y bitShift: bits
]

{ #category : #'truncation and round off' }
Point >> ceiling [
	"Answer a Point that is the receiver's x and y ceiling. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self ].
	^ x ceiling @ y ceiling

]

{ #category : #comparing }
Point >> closeTo: aPoint [
 	^ (x closeTo: aPoint x) and: [ y closeTo: aPoint y ]
]

{ #category : #'rectangle creation' }
Point >> corner: aPoint [ 
	"Answer a Rectangle whose origin is the receiver and whose corner is aPoint. This is one of the infix ways of expressing the creation of a rectangle."

	^ Rectangle origin: self corner: aPoint
]

{ #category : #'point functions' }
Point >> crossProduct: aPoint [ 
	"Answer a number that is the cross product of the receiver and the argument, aPoint."

	^ (x * aPoint y) - (y * aPoint x)
]

{ #category : #copying }
Point >> deepCopy [
	"Implemented here for better performance."

	^ x deepCopy @ y deepCopy
]

{ #category : #'point functions' }
Point >> directionToLineFrom: p1 to: p2 [
	"Answer the direction of the line from the receiver position.
	< 0 => left (receiver to right)
	= => on line
	> 0 => right (receiver to left)."

	^((p2 x - p1 x) * (self y - p1 y)) - ((self x - p1 x) * (p2 y - p1 y))
]

{ #category : #'point functions' }
Point >> dotProduct: aPoint [ 
	"Answer a number that is the dot product of the receiver and the argument, aPoint. That is, the two points are multipled and the coordinates of the result summed."

	^ (x * aPoint x) + (y * aPoint y)
]

{ #category : #'point functions' }
Point >> eightNeighbors [
	^ { self + (1 @ 0) . self + (1 @ 1) .  self + (0 @ 1) . self + (-1 @ 1) . self + (-1 @ 0) .
		 self + (-1 @ -1) . self + (0 @ -1) . self + (1 @ -1)}

]

{ #category : #'rectangle creation' }
Point >> extent: aPoint [ 
	"Answer a Rectangle whose origin is the receiver and whose extent is aPoint. This is one of the infix ways of expressing the creation of a rectangle."

	^ Rectangle origin: self extent: aPoint
]

{ #category : #'point functions' }
Point >> flipBy: direction centerAt: c [
	"Answer a Point which is flipped according to the direction about the point c. Direction must be #vertical or #horizontal."
	direction == #vertical ifTrue: [ ^ x @ (c y * 2 - y) ].
	direction == #horizontal ifTrue:  [^ (c x * 2 - x) @ y ].
	self error: 'unrecognizable direction'
]

{ #category : #'truncation and round off' }
Point >> floor [
	"Answer a Point that is the receiver's x and y floor. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self ].
	^ x floor @ y floor

]

{ #category : #'point functions' }
Point >> fourDirections [
	"Return vertices for a square centered at 0 asPoint with the receiver as first corner.
	Returns the four rotation of the reciever in counter clockwise order with the receiver appearing last."
	
	^ Array with: self leftRotated
			with: self negated
			with: self rightRotated
			with: self 
		

]

{ #category : #'point functions' }
Point >> fourNeighbors [
	^ Array with: self + (1 @ 0)
		with: self + (0 @ 1)
		with: self + (-1 @ 0)
		with: self + (0 @ -1)

]

{ #category : #'point functions' }
Point >> grid: aPoint [ 
	"Answer a Point to the nearest rounded grid modules specified by aPoint."
	| newX newY |
	newX := x + (aPoint x // 2) truncateTo: aPoint x.
	newY := y + (aPoint y // 2) truncateTo: aPoint y.
	^ newX @ newY
]

{ #category : #'extent functions' }
Point >> guarded [
	"Return a positive nonzero extent."
	self max: 1@1
]

{ #category : #comparing }
Point >> hash [
	"Hash is reimplemented because = is implemented."

	^ ( x hash hashMultiply + y hash) hashMultiply
]

{ #category : #'point functions' }
Point >> insideTriangle: p1 with: p2 with: p3 [ 
	"Return true if the receiver is within the triangle defined by the three coordinates.
	Note: This method computes the barycentric coordinates for the receiver and tests those coordinates."

	| p0 b0 b1 b2 b3 |
	p0 := self.
	b0 := (p2 x - p1 x) * (p3 y - p1 y) - ((p3 x - p1 x) * (p2 y - p1 y)).
	b0 isZero ifTrue: [ ^ false ].	"degenerate"
	b0 := 1.0 / b0.
	b1 := ((p2 x - p0 x) * (p3 y - p0 y) - ((p3 x - p0 x) * (p2 y - p0 y))) * b0.
	b2 := ((p3 x - p0 x) * (p1 y - p0 y) - ((p1 x - p0 x) * (p3 y - p0 y))) * b0.
	b3 := ((p1 x - p0 x) * (p2 y - p0 y) - ((p2 x - p0 x) * (p1 y - p0 y))) * b0.
	b1 < 0.0 ifTrue: [ ^ false ].
	b2 < 0.0 ifTrue: [ ^ false ].
	b3 < 0.0 ifTrue: [ ^ false ].
	^ true
]

{ #category : #interpolating }
Point >> interpolateTo: end at: amountDone [
	"Interpolate between the instance and end after the specified amount has been done (0 - 1)."

	^ self * (1 - amountDone) + (end * amountDone)
]

{ #category : #testing }
Point >> isFloatPoint [
	"Return true if both of my x and y coordinates
	are represented by float values, otherwise false"
	^ x isFloat and: [ y isFloat ]
]

{ #category : #geometry }
Point >> isInsideCircle: a with: b with: c [ 
	"Returns true if self is inside the circle defined by the points a, b, c. See Guibas and Stolfi (1985) p.107"
	^ (a dotProduct: a)
		* (b triangleArea: c with: self) - ((b dotProduct: b)
			* (a triangleArea: c with: self)) + ((c dotProduct: c)
			* (a triangleArea: b with: self)) - ((self dotProduct: self)
			* (a triangleArea: b with: c)) > 0.0
]

{ #category : #testing }
Point >> isIntegerPoint [
	"Return true if both of my x and y coordinates
	are represented by integer values, otherwise false"
	^ x isInteger and: [ y isInteger ] 
]

{ #category : #testing }
Point >> isPoint [
	^ true
]

{ #category : #'self evaluating' }
Point >> isSelfEvaluating [
	^ self class == Point
]

{ #category : #testing }
Point >> isZero [
	^ x isZero and: [ y isZero ]
]

{ #category : #'point functions' }
Point >> leftRotated [
	"Return the receiver rotated 90 degrees. i.e., self rotateBy: #left centerAt: 0 asPoint. Compare to transposed and normal."
	^ y  @x negated
]

{ #category : #arithmetic }
Point >> max [
	"Answer a number that is the maximum of the x and y of the receiver."

	^self x max: self y
]

{ #category : #comparing }
Point >> max: aPoint [ 
	"Answer the lower right corner of the rectangle uniquely defined by the receiver and the argument, aPoint."

	^ (x max: aPoint x) @ (y max: aPoint y)
]

{ #category : #arithmetic }
Point >> min [
	"Answer a number that is the minimum of the x and y of the receiver."

	^ self x min: self y
]

{ #category : #comparing }
Point >> min: aPoint [ 
	"Answer the upper left corner of the rectangle uniquely defined by the receiver and the argument, aPoint."

	^ (x min: aPoint x) @ (y min: aPoint y)
]

{ #category : #comparing }
Point >> min: aMin max: aMax [ 

	^ (self min: aMin) max: aMax
]

{ #category : #'point functions' }
Point >> nearestPointAlongLineFrom: p1 to: p2 [ 
	"Note this will give points beyond the endpoints."
	
	| x21 y21 t x1 y1 |
	p1 x = p2 x ifTrue: [ ^ p1 x @ y ].
	p1 y = p2 y ifTrue: [ ^ x @ p1 y ].
	x1 := p1 x asFloat.
	y1 := p1 y asFloat.
	x21 := p2 x asFloat - x1.
	y21 := p2 y asFloat - y1.
	t := ((y asFloat - y1) / x21 + ((x asFloat - x1) / y21)) / (x21 / y21 + (y21 / x21)).
	^ (x1 + (t * x21)) @ (y1 + (t * y21))
	
]

{ #category : #'point functions' }
Point >> nearestPointOnLineFrom: p1 to: p2 [
	"This will not give points beyond the endpoints"
	^ (self nearestPointAlongLineFrom: p1 to: p2)
		adhereTo: (p1 rectangle: p2)
]

{ #category : #transforming }
Point >> negated [
	"Answer a point whose x and y coordinates are the negatives of those of the receiver."
	^ (0 - x) @ (0 - y)
]

{ #category : #'point functions' }
Point >> octantOf: otherPoint [ 
	"Return 1..8 indicating relative direction to otherPoint.  
	1=ESE, 2=SSE, ... etc. clockwise to 8=ENE"

	| quad moreHoriz |
	(x = otherPoint x and: [ y > otherPoint y ]) ifTrue: [ ^ 6 ].
	"special case"
	(y = otherPoint y and: [ x < otherPoint x ]) ifTrue: [ ^ 8 ].
	quad := self quadrantOf: otherPoint.
	moreHoriz := (x - otherPoint x) abs >= (y - otherPoint y) abs.
	^ (quad even eqv: moreHoriz) 
		ifTrue: [ quad * 2 ]
		ifFalse: [ quad * 2 - 1 ]
]

{ #category : #printing }
Point >> printOn: aStream [ 
	"The receiver prints on aStream in terms of infix notation."
	
	aStream nextPut: $(.
	x printOn: aStream.
	aStream nextPut: $@.
	(y notNil and: [y negative])
		ifTrue: [
			"Avoid ambiguous @- construct"
			aStream space].
	y printOn: aStream.
	aStream nextPut: $).
]

{ #category : #'point functions' }
Point >> quadrantOf: otherPoint [
	"Return 1..4 indicating relative direction to otherPoint.
	1 is downRight, 2=downLeft, 3=upLeft, 4=upRight"
	^ x <= otherPoint x
		ifTrue: [ y < otherPoint y ifTrue: [1] ifFalse: [4] ]
		ifFalse: [ y <= otherPoint y ifTrue: [2] ifFalse: [3] ]

]

{ #category : #arithmetic }
Point >> reciprocal [
	"Answer a Point with coordinates that are the reciprocals of mine."

	^ x reciprocal @ y reciprocal.

]

{ #category : #'rectangle creation' }
Point >> rectangle: aPoint [ 
	"Answer a Rectangle that encompasses the receiver and aPoint. This is the most general infix way to create a rectangle."

	^ Rectangle 
		point: self
		point: aPoint
]

{ #category : #'point functions' }
Point >> reflectedAbout: aPoint [
	"Answer a new point that is the reflection of the receiver about the given point."

	^ (self - aPoint) negated + aPoint
]

{ #category : #'point functions' }
Point >> rightRotated [
	"Return the receiver rotated 90 degrees, i.e. self rotateBy: #right centerAt: 0 asPoint. Compare to transposed and normal."
	^ y negated @ x
]

{ #category : #'truncation and round off' }
Point >> roundDownTo: grid [
	"Answer a Point that is the receiver's x and y rounded to grid x and 
	grid y by lower value (toward negative infinity)."
	
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x roundDownTo: gridPoint x) @ (y roundDownTo: gridPoint y)
]

{ #category : #'truncation and round off' }
Point >> roundTo: grid [
	"Answer a Point that is the receiver's x and y rounded to grid x and 
	grid y."
	
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x roundTo: gridPoint x) @ (y roundTo: gridPoint y)
]

{ #category : #'truncation and round off' }
Point >> roundUpTo: grid [
	"Answer a Point that is the receiver's x and y rounded to grid x and 
	grid y by upper value (toward infinity)."
	
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x roundUpTo: gridPoint x) @ (y roundUpTo: gridPoint y)
]

{ #category : #'truncation and round off' }
Point >> rounded [
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self] .
	^ x rounded @ y rounded

]

{ #category : #transforming }
Point >> scaleBy: factor [ 
	"Answer a Point scaled by factor (an instance of Point)."

	^(factor x * x) @ (factor y * y)
]

{ #category : #transforming }
Point >> scaleFrom: rect1 to: rect2 [
	"Produce a point stretched according to the stretch from rect1 to rect2"
	^ rect2 topLeft + (((x-rect1 left) * rect2 width // rect1 width)
					@ ((y-rect1 top) * rect2 height // rect1 height))
]

{ #category : #transforming }
Point >> scaleTo: anExtent [
	"Return a Point scalefactor for shrinking a thumbnail of the receiver's extent to fit within anExtent. self and anExtent are expected to have positive nonZero x and y."
    "(200@200 scaleTo: 400@400) >>> (2.0@2.0)"
	 "(40@40 scaleTo: 400@400) >>> (10.0@10.0)"
	
    | factor  sX sY | 
    factor :=  3.0 reciprocal. "EccentricityThreshhold reciprical"
    sX := anExtent x / self  x asFloat.
    sY :=  anExtent y / self  y asFloat.
    sX = sY ifTrue: [ ^ sX @ sY ]. "Same aspect ratio"
    ^ sX < sY 
        ifTrue: [ sX @ (sX max: sY * factor) ] 
        ifFalse: [ (sY max: sX * factor ) @ sY  ]
]

{ #category : #private }
Point >> setX: xValue setY: yValue [ 
	x := xValue.
	y := yValue
]

{ #category : #geometry }
Point >> sideOf: otherPoint [ 
	"Returns #left, #right or #center if the otherPoint lies to the left, right or on the line given by the vector from 0@0 to self"
	| side |
	side := (self crossProduct: otherPoint) sign.
	^ { #right . #center . #left } at: side + 2
]

{ #category : #'point functions' }
Point >> sign [
	^ (x sign @ y sign)
]

{ #category : #'point functions' }
Point >> sortsBefore: otherPoint [
	"Return true if the receiver sorts before the other point"
	^ y = otherPoint y
		ifTrue: [ x <= otherPoint x ]
		ifFalse: [ y <= otherPoint y ]
]

{ #category : #'point functions' }
Point >> squaredDistanceTo: aPoint [ 
	"Answer the distance between aPoint and the receiver."
	| delta |
	delta := aPoint - self.
	^ delta dotProduct: delta
]

{ #category : #printing }
Point >> storeOn: aStream [ 
	"x@y printed form is good for storing too"
	aStream nextPut: $(.
	self printOn: aStream.
	aStream nextPut: $).

]

{ #category : #geometry }
Point >> to: end1 intersects: start2 to: end2 [ 
	"Returns true if the linesegment from start1 (=self) to end1 intersects with the segment from start2 to end2, otherwise false."
	
	| start1 sideStart sideEnd |
	start1 := self.
	(((start1 = start2 or: [ end1 = end2 ]) or: [ start1 = end2 ]) or: [ start2 = end1 ]) ifTrue: [ ^ true ].
	sideStart := start1 
		to: end1
		sideOf: start2.
	sideEnd := start1 
		to: end1
		sideOf: end2.
	sideStart = sideEnd ifTrue: [ ^ false ].
	sideStart := start2 
		to: end2
		sideOf: start1.
	sideEnd := start2 
		to: end2
		sideOf: end1.
	sideStart = sideEnd ifTrue: [ ^ false ].
	^ true
]

{ #category : #geometry }
Point >> to: end sideOf: otherPoint [ 
	"Returns #left, #right, #center if the otherPoint lies to the left, right or on the line given by the vector from self to end"
	^ end - self sideOf: otherPoint - self
]

{ #category : #transforming }
Point >> translateBy: delta [ 
	"Answer a Point translated by delta (an instance of Point)."

	^ (delta x + x) @ (delta y + y)
]

{ #category : #'point functions' }
Point >> transposed [
	^ y @ x
]

{ #category : #geometry }
Point >> triangleArea: b with: c [
	"Returns twice the area of the oriented triangle (a, b, c), i.e., the area is positive if the triangle is oriented counterclockwise"
	^ b x - self x * (c y - self y) - (b y - self y * (c x - self x))
]

{ #category : #'truncation and round off' }
Point >> truncateTo: grid [ 
	"Answer a Point that is the receiver's x and y truncated to grid x and grid y."
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x truncateTo: gridPoint x) @ (y truncateTo: gridPoint y)
]

{ #category : #'truncation and round off' }
Point >> truncated [
	"Answer a Point whose x and y coordinates are integers. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self ].
	^ x truncated @ y truncated

]

{ #category : #copying }
Point >> veryDeepCopyWith: deepCopier [
	"Return self.  I am immutable in the Morphic world.  Do not record me."
	^ self
]

{ #category : #accessing }
Point >> x [
	"Answer the x coordinate."

	^ x
]

{ #category : #accessing }
Point >> y [
	"Answer the y coordinate."

	^ y
]
