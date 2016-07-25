comparing
closenessVectorForStretch: stretch slant: slant weight: weight
	| normalizedSlant |
	normalizedSlant := slant.
	normalizedSlant ~= 0 ifTrue:[
		 "treat italic and oblique as though they were they same"
		normalizedSlant := LogicalFont slantItalic].
	^{(stretch - LogicalFont stretchRegular) * 11. slant * 7. ((weight - LogicalFont weightRegular) / 100) * 5}