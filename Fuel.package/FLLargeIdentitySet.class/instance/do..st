enumerating
do: aBlock

	tally = 0 ifTrue: [ ^self ].
	includesNil ifTrue: [ aBlock value: nil ].
	1 to: 4096 do: [ :arrayIndex |
		| subArray |
		subArray := array at: arrayIndex.
		1 to: (tallies at: arrayIndex) do: [ :index |
			aBlock value: (subArray at: index) ] ]