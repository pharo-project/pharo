initialization
on: aCollection from: firstIndex to: lastIndex
	"Check the header of the GZIP stream."
	| method magic flags length |
	super on: aCollection from: firstIndex to: lastIndex.
	crc := 16rFFFFFFFF.
	magic := self nextBits: 16.
	(magic = GZipMagic) 
		ifFalse:[^self error:'Not a GZipped stream'].
	method := self nextBits: 8.
	(method = GZipDeflated)
		ifFalse:[^self error:'Bad compression method'].
	flags := self nextBits: 8.
	(flags anyMask: GZipEncryptFlag) 
		ifTrue:[^self error:'Cannot decompress encrypted stream'].
	(flags anyMask: GZipReservedFlags)
		ifTrue:[^self error:'Cannot decompress stream with unknown flags'].
	"Ignore stamp, extra flags, OS type"
	self nextBits: 16; nextBits: 16. "stamp"
	self nextBits: 8. "extra flags"
	self nextBits: 8. "OS type"
	(flags anyMask: GZipContinueFlag) "Number of multi-part archive - ignored"
		ifTrue:[self nextBits: 16]. 
	(flags anyMask: GZipExtraField) "Extra fields - ignored"
		ifTrue:[	length := self nextBits: 16.
				1 to: length do:[:i| self nextBits: 8]].
	(flags anyMask: GZipNameFlag) "Original file name - ignored"
		ifTrue:[[(self nextBits: 8) = 0] whileFalse].
	(flags anyMask: GZipCommentFlag) "Comment - ignored"
		ifTrue:[[(self nextBits: 8) = 0] whileFalse].
