enumeration
valuesDo: aBlock 
	"See comments in Dictionary>>valuesDo:.  The code keeps the key so it's
	not collected during the evaluation of aBlock"

	tally = 0 ifTrue: [ ^self ].
	1 to: array size do:
		[ :eachIndex | 
			| eachAssociation eachKey |
			eachAssociation := array at: eachIndex.
			nil == eachAssociation ifFalse:
				[
					eachKey := eachAssociation key.
					nil == eachKey ifFalse: [ aBlock value: eachAssociation value ]
				]
		]