Extension { #name : #Point }

{ #category : #'*Math-Operations-Extensions' }
Point >> angle [
	"Answer the angle in radians between the vectors represented by the receiver and (1, 0) from the origin."

	^ self y arcTan: self x
]

{ #category : #'*Math-Operations-Extensions' }
Point >> angleWith: aPoint [
	"Answer the angle in radians between the vectors represented by the receiver and aPoint from the origin."

	| ar ap |
	ar := self angle.
	ap := aPoint angle.
	^ ap >= ar
		ifTrue: [ ap - ar ]
		ifFalse: [ Float pi * 2 - ar + ap ]
]

{ #category : #'*Math-Operations-Extensions' }
Point >> bearingToPoint: anotherPoint [
    "Return the bearing, in degrees, from the receiver to anotherPoint."

    | deltaX deltaY  |
    deltaX := anotherPoint x -  x.
    deltaY := anotherPoint y - y.
    deltaX abs < 0.001
        ifTrue: [ ^ deltaY > 0 ifTrue: [ 180 ] ifFalse: [ 0 ]].
    ^ ((deltaX >= 0 ifTrue: [90] ifFalse: [270])
            - ((deltaY / deltaX) arcTan negated radiansToDegrees)) rounded

]

{ #category : #'*Math-Operations-Extensions' }
Point >> degrees [
	"Answer the angle the receiver makes with origin in degrees. right is 0; down is 90."
	| tan theta |
	^ x = 0 
		ifTrue: 
			[ y >= 0 
				ifTrue: [ 90.0 ]
				ifFalse: [ 270.0 ] ]
		ifFalse: 
			[ tan := y asFloat / x asFloat.
			theta := tan arcTan.
			x >= 0 
				ifTrue: 
					[ y >= 0 
						ifTrue: [ theta radiansToDegrees ]
						ifFalse: [ 360.0 + theta radiansToDegrees ] ]
				ifFalse: [ 180.0 + theta radiansToDegrees ] ]
]

{ #category : #'*Math-Operations-Extensions' }
Point >> dist: aPoint [ 
	"Answer the distance between aPoint and the receiver."
	| dx dy |
	dx := aPoint x - x.
	dy := aPoint y - y.
	^ (dx * dx + (dy * dy)) sqrt
]

{ #category : #'*Math-Operations-Extensions' }
Point >> distanceTo: aPoint [ 
	"Answer the distance between aPoint and the receiver."
	| dx dy |
	dx := aPoint x - x.
	dy := aPoint y - y.
	^ (dx * dx + (dy * dy)) sqrt
]

{ #category : #'*Math-Operations-Extensions' }
Point >> normal [
	"Answer a Point representing the unit vector rotated 90 deg clockwise. For the zero point return -1@0."

	| n d |
	n := y negated @ x.
	(d := (n x * n x + (n y * n y))) = 0
		 ifTrue: [ ^ -1 @0 ].
	^ n / d sqrt
]

{ #category : #'*Math-Operations-Extensions' }
Point >> normalized [
	"Optimized for speed"
	| r |
	r := (x * x + (y * y)) sqrt.
	^ (x / r) @ (y / r)
]

{ #category : #'*Math-Operations-Extensions' }
Point >> onLineFrom: p1 to: p2 [
	^ self onLineFrom: p1 to: p2 within: 2
]

{ #category : #'*Math-Operations-Extensions' }
Point >> onLineFrom: p1 to: p2 within: epsilon [
	"Answer true if the receiver lies on the given line segment between p1 and p2 within a small epsilon."

	"is this point within the box spanning p1 and p2 expanded by epsilon? (optimized)"
	p1 x < p2 x
		ifTrue: [
			((x < (p1 x - epsilon)) or: [x > (p2 x + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((x < (p2 x - epsilon)) or: [x > (p1 x + epsilon)]) ifTrue: [^ false]].
	p1 y < p2 y
		ifTrue: [
			((y < (p1 y - epsilon)) or: [y > (p2 y + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((y < (p2 y - epsilon)) or: [y > (p1 y + epsilon)]) ifTrue: [^ false]].

	"it's in the box; is it on the line?"
	^ (self distanceTo: (self nearestPointAlongLineFrom: p1 to: p2)) <= epsilon
]

{ #category : #'*Math-Operations-Extensions' }
Point >> r [
	"Answer the receiver's radius in polar coordinate system."

	^ (self dotProduct: self) sqrt
]

{ #category : #'*Math-Operations-Extensions' }
Point class >> r: rho degrees: degrees [
	"Answer an instance of me with polar coordinates rho and theta."

	^ self basicNew setR: rho degrees: degrees
]

{ #category : #'*Math-Operations-Extensions' }
Point >> rotateBy: angle about: center [ 
	"This method returns the point obtained after rotating myself (counter clockwise) 
	around the #center point of an #angle given as parameter. The #angle provided as 
	parameter is interpreted as being in radian."
	| p r theta |
	p := self - center.
	r := p r.
	theta := angle asFloat - p theta.
	^ (center x asFloat + (r * theta cos)) @ (center y asFloat - (r * theta sin))
]

{ #category : #'*Math-Operations-Extensions' }
Point >> rotateBy: direction centerAt: c [ 
	"Answer a Point which is rotated according to direction, about the point c.
	Direction must be one of #right (CW), #left (CCW) or #pi (180 degrees)."
	| offset |
	offset := self - c.
	direction == #right ifTrue: [ ^ offset y negated @ offset x + c ].
	direction == #left ifTrue: [ ^ offset y @ offset x negated + c ].
	direction == #pi ifTrue: [ ^ c - offset ].
	self error: 'unrecognizable direction'
]

{ #category : #'*Math-Operations-Extensions' }
Point >> setR: rho degrees: degrees [ 
	| radians |
	radians := degrees asFloat degreesToRadians.
	x := rho asFloat * radians cos.
	y := rho asFloat * radians sin
]

{ #category : #'*Math-Operations-Extensions' }
Point >> theta [
	"Answer the angle the receiver makes with origin in radians. right is 0; 
	down is 90."

	| tan theta |
	^ x = 0
		ifTrue: [y >= 0
				ifTrue: [ 1.570796326794897 "90.0 degreesToRadians" ]
				ifFalse: [ 4.71238898038469 "270.0 degreesToRadians" ] ]
		ifFalse: 
			[tan := y asFloat / x asFloat.
			theta := tan arcTan.
			x >= 0
				ifTrue: [y >= 0
						ifTrue: [ theta ]
						ifFalse: [ "360.0 degreesToRadians" 6.283185307179586 + theta ]]
				ifFalse: [ "180.0 degreesToRadians" 3.141592653589793 + theta ] ]
]
