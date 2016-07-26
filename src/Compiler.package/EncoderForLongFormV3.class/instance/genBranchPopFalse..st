bytecode generation
genBranchPopFalse: distance
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	distance < 1024 ifTrue:
		["172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
		 stream
			nextPut: 172 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023