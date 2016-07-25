enumerating
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	tally = 0 ifTrue: [^ self].
	array do:
		[:each |
		each ifNotNil: [aBlock value: each]]