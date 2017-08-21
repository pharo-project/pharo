writting
writeInt: aNumber
	| numberToSave |
	numberToSave := aNumber < 0
		ifTrue: [ (0 - aNumber) bitInvert32 + 1 ]
		ifFalse: [ aNumber ].
	1 to: 4 do: [ :i | stream nextPut: (numberToSave digitAt: i) ]