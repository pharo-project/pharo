polar coordinates
theta
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