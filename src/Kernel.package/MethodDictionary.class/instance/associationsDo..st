enumeration
associationsDo: aBlock 

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil:
			[ :key | aBlock value: (Association key: key value: (array at: i))]]