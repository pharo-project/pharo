inflating
processStoredBlock
	| chkSum length |
	"Skip to byte boundary"
	self nextBits: (bitPos bitAnd: 7).
	length := self nextBits: 16.
	chkSum := self nextBits: 16.
	(chkSum bitXor: 16rFFFF) = length
		ifFalse:[^self error:'Bad block length'].
	litTable := nil.
	distTable := length.
	state := state bitOr: BlockProceedBit.
	^self proceedStoredBlock