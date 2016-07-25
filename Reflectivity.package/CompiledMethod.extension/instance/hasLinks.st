*Reflectivity
hasLinks
	self reflectiveMethod ifNil: [ ^false ].
	^self reflectiveMethod hasLinks. 