private
doesLiteralArrayContainComma: aLiteral 
	aLiteral class = Array 
		ifFalse: [ ^ false ].
	(aLiteral includes: #,)
		ifTrue: [ ^ true ].
	^ aLiteral 
		anySatisfy: [ :each | self doesLiteralArrayContainComma: each ]