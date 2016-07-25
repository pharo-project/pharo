bytecode generation
genSend: selectorLiteralIndex numArgs: nArgs
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		["No special selector sends in long form."
		^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments"
		 stream
			nextPut: 131;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 64 and: [nArgs < 4]) ifTrue: 
	 	["In Squeak V3
			134 	10000110 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk To Superclass With jjjjjjjj Arguments
		 is replaced by
			134 	10000110 jjkkkkkk 	Send Literal Selector #kkkkkk With jj Arguments"
		 stream
			nextPut: 134;
			nextPut: ((nArgs bitShift: 6) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex <= 255 and: [nArgs <= 31]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs > 31 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex > 255 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]