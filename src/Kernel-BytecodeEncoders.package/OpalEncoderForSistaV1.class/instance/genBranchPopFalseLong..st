bytecode generation
genBranchPopFalseLong: distance
	"239		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)	"
	| distanceMod256 |
	(distance < 0 or: [distance > 32767]) ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 32767].
	distanceMod256 := (distance < 0 or: [distance > 255])
							ifTrue:
								[self genUnsignedSingleExtendB: (distance bitShift: -8).
								 distance bitAnd: 255]
							ifFalse: [distance].
	stream
		nextPut: 239;
		nextPut: distanceMod256