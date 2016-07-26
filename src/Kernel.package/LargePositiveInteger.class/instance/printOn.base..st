printing
printOn: aStream base: b
	"Append a representation of this number in base b on aStream.
	In order to reduce cost of LargePositiveInteger ops, split the number in approximately two equal parts in number of digits."
	
	| halfDigits halfPower head tail nDigitsUnderestimate |
	"Don't engage any arithmetic if not normalized"
	(self digitLength = 0 or: [(self digitAt: self digitLength) = 0]) 
		ifTrue: [ ^self normalize printOn: aStream base: b ].
	
	nDigitsUnderestimate := b = 10
		ifTrue: [((self highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [self highBit quo: b highBit].
		
	"splitting digits with a whole power of two is more efficient"
	halfDigits := 1 bitShift: nDigitsUnderestimate highBit - 2.
	
	halfDigits <= 1
		ifTrue: ["Hmmm, this could happen only in case of a huge base b... Let lower level fail"
			^self printOn: aStream base: b nDigits: (self numberOfDigitsInBase: b)].
	
	"Separate in two halves, head and tail"
	halfPower := b raisedToInteger: halfDigits.
	head := self quo: halfPower.
	tail := self - (head * halfPower).
	
	"print head"
	head printOn: aStream base: b.
	
	"print tail without the overhead to count the digits"
	tail printOn: aStream base: b nDigits: halfDigits