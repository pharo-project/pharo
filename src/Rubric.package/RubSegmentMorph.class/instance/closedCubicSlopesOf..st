cubic support
closedCubicSlopesOf: knots 
	"Sent to knots returns the slopes of a closed cubic spline.
	From the same set of java sources as naturalCubic. This is a smalltalk  
	transliteration of the java code."
	"from java code NatCubicClosed extends NatCubic  
	solves for the set of equations for all knots: 
	b1+4*b2+b3=3*(a3-a1)
	where a1 is (knots atWrap: index + 1) etc.
	and the b's are the slopes .
	by decomposing the matrix into upper triangular and lower matrices  
	and then back sustitution. See Spath 'Spline Algorithms for Curves  
	and Surfaces' pp 19--21. The D[i] are the derivatives at the knots.  
	"
	
	| v w x y z n1  D F G H |
	n1 := knots size.
	n1 < 3
		ifTrue: [self error: 'Less than 3 points makes a poor curve'].
	v := Array new: n1.
	w := Array new: n1.
	y := Array new: n1.
	
	D := Array new: n1.
	x := knots.
	z := 1.0 / 4.0.
	v at: 2 put: z.
	w at: 2 put: z.
	y at: 1 put: z * 3.0 * ((x at: 2) - (x at: n1)).
	H := 4.0.
	F := 3 * ((x at: 1) - (x at: n1 - 1)).
	G := 1.
	2 to: n1 - 1 do: [:k | 
			z := 1.0 / (4.0 - (v at: k)).
			v at: k + 1 put: z.
			w at: k + 1 put: z negated
					* (w at: k).
			y at: k put: z * (3.0 * ((x at: k + 1) - (x at: k - 1)) - (y at: k - 1)).
			H := H - (G * (w at: k)).
			F := F - (G * (y at: k - 1)).
			G := (v at: k) negated * G].
	H := H - (G + 1 * ((v at: n1) + (w at: n1))).
	y at: n1 put: F - (G + 1 * (y at: n1 - 1)).
	D at: n1 put: (y at: n1) / H.
	D at: n1 - 1 put: (y at: n1 - 1) - ((v at: n1) + (w at: n1) * (D at: n1)).
	(1 to: n1 - 2)
		reverseDo: [:k | D at: k put: (y at: k)
					- ((v at: k + 1) * (D at: k + 1)) - ((w at: k + 1) * (D at: n1))].
	^ D 

