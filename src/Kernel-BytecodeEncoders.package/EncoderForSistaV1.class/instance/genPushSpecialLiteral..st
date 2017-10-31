bytecode generation
genPushSpecialLiteral: aLiteral
	"77			01001101			Push true
	 78			01001110			Push false
	 79			01001111			Push nil
	 80			01010000			Push 0
	 81			01010001			Push 1
	 232		11101000	iiiiiiii		Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	| index |
	aLiteral isInteger ifTrue:
		[aLiteral == 0 ifTrue:
			[stream nextPut: 80.
			 ^self].
		 aLiteral == 1 ifTrue:
			[stream nextPut: 81.
			 ^self].
		 ^self genPushInteger: aLiteral].
	index := #(true false nil)
					indexOf: aLiteral
					ifAbsent: [^self error: 'push special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 76 + index