bytecode generation
genBranchPopFalse: distance
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	(distance > 0 and: [distance < 9]) ifTrue:
		["152-159 	10011iii 	Pop and Jump 0n False iii +1 (i.e., 1 through 8)"
		 stream nextPut: 152 + distance - 1.
		 ^self].
	distance < 1024 ifTrue:
		["172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
		 stream
			nextPut: 172 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023