scanning
method: method refersInBytecodeToLiteral: aLiteral specialSelectorIndex: specialOrNil
	"Answer if method refers to the literal aLiteral in the bytecode, as opposed to in its literal frame."

	"	77			01001101				Push true
		78			01001110				Push false
		79			01001111				Push nil
		80			01010000				Push 0
		81			01010001				Push 1
		88-91		010110 ii				Return Receiver/true/false/nil
		93			01011101				BlockReturn nil
		96-111		0110 iiii				Send Arithmetic Message #iiii #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr:)
		112-119	01110 iii				Send Special Message #iii #(#at: #at:put: #size #next #nextPut: #atEnd #'==' class)
		120		01111000				UNASSIGNED (was: blockCopy:)
		121		01111001				Send Special Message #value
		122-123	0111101 i				Send Special Message #i #(#value: #do:)
		124-127	011111 ii				Send Special Message #ii #(#new #new: #x #y))
	*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	*	232		11101000	iiiiiiii		Push Integer #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)
	*	233		11101001	iiiiiiii		Push Character #iiiiiiii (+ Extend B * 256)
		249		11111001 	xxxxxxxx	syyyyyyy	Reserved for Push Float"
	| byte extended scanner |
	specialOrNil ifNotNil:
		[byte := specialOrNil + 95.
		^(InstructionStream on: method) scanFor: [:b| b = byte]].
	extended := false.
	aLiteral isInteger ifTrue:
		[(aLiteral >= -32768 and: [aLiteral <= 32767]) ifFalse: [^false].
		 scanner := InstructionStream on: method.
		 (aLiteral >= 0 and: [aLiteral <= 255]) ifTrue:
			[aLiteral <= 1 ifTrue:
				[byte := aLiteral + 80.
				 ^scanner scanFor: [:b| b = byte]].
			 ^scanner scanFor:
				[:b|
				(b = 232
				 and: [extended not
				 and: [scanner followingByte = aLiteral]])
				or: [extended := b = 225.
					false]]].
		 byte := (aLiteral bitShift: -8) bitAnd: 255.
		^scanner scanFor:
			[:b|
			(b = 232
			 and: [extended
			 and: [scanner followingByte = (aLiteral bitAnd: 255)]])
			or: [extended := b = 225 and: [scanner followingByte = byte].
				false]]].
	aLiteral isCharacter ifTrue:
		[aLiteral asciiValue <= 65535 ifFalse: [^false].
		 scanner := InstructionStream on: method.
		 aLiteral asciiValue <= 255 ifTrue:
			[^scanner scanFor:
				[:b|
				(b = 233
				 and: [extended not
				 and: [scanner followingByte = aLiteral]])
				or: [extended := b = 225.
					false]]].
		 byte := (aLiteral bitShift: -8) bitAnd: 255.
		^scanner scanFor:
			[:b|
			(b = 233
			 and: [extended
			 and: [scanner followingByte = (aLiteral bitAnd: 255)]])
			or: [extended := b = 225 and: [scanner followingByte = byte].
				false]]].
	aLiteral == nil ifTrue:
		[^(InstructionStream on: method) scanFor: [:b| b = 79 or: [b = 91 or: b = 93]]].
	aLiteral == true ifTrue:
		[^(InstructionStream on: method) scanFor: [:b| b = 77 or: [b = 89]]].
	aLiteral == false ifTrue:
		[^(InstructionStream on: method) scanFor: [:b| b = 78 or: [b = 90]]].
	
	^false