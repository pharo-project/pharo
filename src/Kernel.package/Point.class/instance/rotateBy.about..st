transforming
rotateBy: angle about: center 
	"Even though Point.theta is measured CW, this rotates with the more conventional CCW interpretateion of angle."
	| p r theta |
	p := self - center.
	r := p r.
	theta := angle asFloat - p theta.
	^ (center x asFloat + (r * theta cos)) @ (center y asFloat - (r * theta sin))