cubic support
naturalCubicSlopesOf: knots 
	"Sent to knots returns the slopes of a natural cubic curve fit."
	"We solve the equation for knots with end conditions:  
	2*b1+b2 = 3(a1 - a0) 
	bN1+2*bN = 3*(aN-aN1)
	and inbetween:
	b2+4*b3+b4=3*(a4-a2)
	where a2 is (knots atWrap: index + 1) etc.
	and the b's are the slopes .
	N is the last index (knots size)
	N1 is N-1.
	 
	by using row operations to convert the matrix to upper  
	triangular and then back sustitution. The D[i] are the derivatives at the  
	knots."
	
	| x gamma delta D n1 |
	n1 := knots size.
	n1 < 3
		ifTrue: [self error: 'Less than 3 points makes a poor curve'].
	x := knots.
	gamma := Array new: n1.
	delta := Array new: n1.
	
	D := Array new: n1.
	gamma at: 1 put: 1.0 / 2.0.
	2 to: n1 - 1 do: [:i | gamma at: i put: 1.0 / (4.0
						- (gamma at: i - 1))].
	gamma at: n1 put: 1.0 / (2.0
				- (gamma at: n1 - 1)).
	delta at: 1 put: 3.0 * ((x at: 2)
				- (x at: 1))
			* (gamma at: 1).
	2 to: n1 - 1 do: [:i | delta at: i put: 3.0 * ((x at: i + 1)
						- (x at: i - 1))
					- (delta at: i - 1)
					* (gamma at: i)].
	delta at: n1 put: 3.0 * ((x at: n1)
				- (x at: n1 - 1))
			- (delta at: n1 - 1)
			* (gamma at: n1).
	D
		at: n1
		put: (delta at: n1).
	(1 to: n1 - 1)
		reverseDo: [:i | D at: i put: (delta at: i)
					- ((gamma at: i)
							* (D at: i + 1))].
	^ D