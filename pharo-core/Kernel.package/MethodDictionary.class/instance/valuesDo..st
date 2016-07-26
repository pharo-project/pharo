enumeration
valuesDo: aBlock 
	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (array at: i) ifNotNil: [ :value | aBlock value: value]]