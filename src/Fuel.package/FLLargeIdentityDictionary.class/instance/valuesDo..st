enumerating
valuesDo: aBlock

	tally = 0 ifTrue: [ ^self ].
	1 to: 4096 do: [ :arrayIndex |
		| valueArray |
		valueArray := values at: arrayIndex.
		1 to: (tallies at: arrayIndex) do: [ :index |
			aBlock value: (valueArray at: index) ] ]