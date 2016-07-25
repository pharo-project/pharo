cubic support
transform: coefficients toCubicPointPolynomialAt: vIndex 
	"From curve information assemble a 4-array of points representing the coefficents for curve segment between to points. Beginning point is first point in array endpoint is the pointSum of the array. Meant to be sent to newcurves idea of curve coefficents." 
	| transformed |
	transformed := (1 to: 4) collect: [:i | 
			((coefficients at: i) at: vIndex) @ ((coefficients at: 4 + i) at: vIndex)].
	^ Cubic withAll: transformed