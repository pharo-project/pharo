enumerating
keysDo: aBlock

	tally = 0 ifTrue: [ ^self ].
	1 to: 4096 do: [ :arrayIndex |
		| keyArray |
		keyArray := keys at: arrayIndex.
		1 to: (tallies at: arrayIndex) do: [ :index |
			aBlock value: (keyArray at: index) ] ]