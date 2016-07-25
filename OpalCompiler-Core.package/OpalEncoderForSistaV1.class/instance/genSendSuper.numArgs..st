bytecode generation
genSendSuper: selectorLiteralIndex numArgs: nArgs
	| extendedIndex extendedNArgs |
	(selectorLiteralIndex < 0 or: [selectorLiteralIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'selectorLiteralIndex' index: selectorLiteralIndex range: 0 to: 65535].
	(nArgs < 0 or: [nArgs > 31]) ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	(extendedIndex := selectorLiteralIndex) > 31 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 32.
		 extendedIndex := extendedIndex \\ 32].
	(extendedNArgs := nArgs) > 7 ifTrue:
		[self genUnsignedSingleExtendB: extendedNArgs // 8.
		 extendedNArgs := extendedNArgs \\ 8].
	"235		11101011	iiiiijjj		Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
	stream
		nextPut: 235;
		nextPut: extendedNArgs + (extendedIndex * 8)