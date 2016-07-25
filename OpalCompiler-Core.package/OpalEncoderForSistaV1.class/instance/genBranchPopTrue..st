bytecode generation
genBranchPopTrue: distance
	(distance > 0 and: [distance < 9]) ifTrue:
		["184-191	10111 iii			Pop and Jump 0n True iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 183 + distance.
		 ^self].
	^self genBranchPopTrueLong: distance