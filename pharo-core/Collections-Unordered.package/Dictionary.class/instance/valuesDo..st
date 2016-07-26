enumerating
valuesDo: aBlock 
	"Evaluate aBlock for each of the receiver's values.  Implemented with == checks
	merely for the sake of maximum efficiency"

	tally = 0 ifTrue: [ ^self ].
	1 to: array size do:
		[ :eachIndex | 
			| eachAssociation |
			eachAssociation := array at: eachIndex.
			nil == eachAssociation ifFalse: [ aBlock value: eachAssociation value ]
		]