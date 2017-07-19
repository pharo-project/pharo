bytecode generation
genSend: selectorLiteralIndex numArgs: nArgs
	| extendedIndex extendedNArgs |
	(selectorLiteralIndex < 0 or: [selectorLiteralIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'selectorLiteralIndex' index: selectorLiteralIndex range: 0 to: 65535].
	(nArgs < 0 or: [nArgs > 31]) ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	(selectorLiteralIndex < 16 and: [nArgs < 3]) ifTrue: 
	 	["128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
		  144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
		  160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments"
		 stream nextPut: 128 + (nArgs * 16) + selectorLiteralIndex.
		 ^self].
	(extendedIndex := selectorLiteralIndex) > 31 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 32.
		 extendedIndex := extendedIndex \\ 32].
	(extendedNArgs := nArgs) > 7 ifTrue:
		[self genUnsignedSingleExtendB: extendedNArgs // 8.
		 extendedNArgs := extendedNArgs \\ 8].
	"234		11101010	i i i i i j j j	Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
	stream
		nextPut: 234;
		nextPut: extendedNArgs + (extendedIndex * 8)