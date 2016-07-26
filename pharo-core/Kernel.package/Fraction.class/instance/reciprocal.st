private
reciprocal
	
	numerator abs = 1 ifTrue: [^denominator * numerator].
	^self class numerator: denominator denominator: numerator