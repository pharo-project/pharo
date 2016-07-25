bytecode generation
genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	"250		11111010 eeiiikkk		jjjjjjjj		Push Closure Num Copied iii (+ Ext A // 16 * 8) Num Args kkk (+ Ext A \\ 16 * 8) BlockSize jjjjjjjj (+ Ext B * 256). ee = num extensions"
	"Including numExtensions makes decoding the bytecode quicker since it obviates having to scan from the beginning of a method."
	| numExtensions numCopiedMod8 numArgsMod8 extA |
	(jumpSize < 0 or: [jumpSize > 65535]) ifTrue:
		[^self outOfRangeError: 'block size' index: jumpSize range: 0 to: 65535].
	(numCopied < 0 or: [numCopied > 127]) ifTrue:
		[^self outOfRangeError: 'num copied' index: numCopied range: 0 to: 127].
	(numArgs < 0 or: [numArgs > 127]) ifTrue:
		[^self outOfRangeError: 'num args' index: numArgs range: 0 to: 127].
	extA := numExtensions := 0.
	(numArgsMod8 := numArgs) > 7 ifTrue:
		[extA := numArgs // 8.
		 numArgsMod8 := numArgsMod8 \\ 8].
	(numCopiedMod8 := numCopied) > 7 ifTrue:
		[extA := extA + (numCopied // 8 * 16).
		 numCopiedMod8 := numCopiedMod8 \\ 8].
	extA ~= 0 ifTrue:
		[self genUnsignedSingleExtendA: extA.
		 numExtensions := 1].
	jumpSize > 255 ifTrue:
		[numExtensions := numExtensions + 1.
		 self genUnsignedSingleExtendB: jumpSize // 256].
	stream
		nextPut: 250;
		nextPut: (numExtensions bitShift: 6) + (numCopiedMod8 bitShift: 3) + numArgsMod8;
		nextPut: (jumpSize bitAnd: 16rFF)