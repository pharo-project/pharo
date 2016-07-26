smoothing
derivs: a first: point1 second: point2 third: point3 
	"Compute the first, second and third derivitives (in coeffs) from
	the Points in this Path (coeffs at: 1 and coeffs at: 5)."

	| len v anArray |
	len := a size.
	len < 2 ifTrue: [^self].
	len > 2 
		ifTrue: 
			[v := Array new: len.
			v at: 1 put: 4.0.
			anArray := Array new: len.
			anArray at: 1 put: 6.0 * (a first - (a second * 2.0) + (a third)).
			2 to: len - 2
				do: 
					[:i | 
					v at: i put: 4.0 - (1.0 / (v at: i - 1)).
					anArray at: i
						put: 6.0 * ((a at: i) - ((a at: i + 1) * 2.0) + (a at: i + 2)) 
								- ((anArray at: i - 1) / (v at: i - 1))].
			point2 at: len - 1 put: (anArray at: len - 2) / (v at: len - 2).
			len - 2 to: 2
				by: 0 - 1
				do: 
					[:i | 
					point2 at: i
						put: ((anArray at: i - 1) - (point2 at: i + 1)) / (v at: i - 1)]].
	point2 at: 1 put: (point2 at: len put: 0.0).
	1 to: len - 1
		do: 
			[:i | 
			point1 at: i
				put: (a at: i + 1) - (a at: i) 
						- (((point2 at: i) * 2.0 + (point2 at: i + 1)) / 6.0).
			point3 at: i put: (point2 at: i + 1) - (point2 at: i)]