bytecode generation
genBranchPopTrueLong: distance
	"238		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0)"
	| distanceMod256 |
	(distance < 0 or: [distance > 32767]) ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 32767].
	(distance > 0 and: [distance < 9]) ifTrue:
		["184-191	10111 iii			Pop and Jump 0n True iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 183 + distance.
		 ^self].
	distanceMod256 := (distance < 0 or: [distance > 255])
							ifTrue:
								[self genUnsignedSingleExtendB: (distance bitShift: -8).
								 distance bitAnd: 255]
							ifFalse: [distance].
	stream
		nextPut: 238;
		nextPut: distanceMod256