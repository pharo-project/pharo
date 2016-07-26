enumerating
valuesDo: aBlock 
	"See comments in Dictionary>>valuesDo:.  The code keeps
	a reference to the value to facilitate debugging"

	tally = 0 ifTrue: [ ^self ].
	1 to: array size do:
		[ :eachIndex | 
			| eachAssociation eachValue |
			eachAssociation := array at: eachIndex.
			nil == eachAssociation ifFalse:
				[
					eachValue := eachAssociation value.
					aBlock value: eachValue
				]
		]