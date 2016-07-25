bytecode generation
genBranchPopFalse: distance
	(distance > 0 and: [distance < 9]) ifTrue:
		["192-199	11000 iii			Pop and Jump 0n False iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 191 + distance.
		 ^self].
	^self genBranchPopFalseLong: distance