encoding
flushBlock: lastBlock
	"Send the current block"
	| lastFlag bitsRequired method bitsSent
	storedLength fixedLength dynamicLength 
	blTree lTree dTree blBits blFreq |

	lastFlag := lastBlock ifTrue:[1] ifFalse:[0].

	"Compute the literal/length and distance tree"
	lTree := ZipEncoderTree buildTreeFrom: literalFreq maxDepth: MaxBits.
	dTree := ZipEncoderTree buildTreeFrom: distanceFreq maxDepth: MaxBits.

	"Compute the bit length tree"
	blBits := lTree bitLengths, dTree bitLengths.
	blFreq := WordArray new: MaxBitLengthCodes.
	self scanBitLengths: blBits into: blFreq.
	blTree := ZipEncoderTree buildTreeFrom: blFreq maxDepth: MaxBitLengthBits.

	"Compute the bit length for the current block.
	Note: Most of this could be computed on the fly but it's getting
	really ugly in this case so we do it afterwards."
	storedLength := self storedBlockSize.
	fixedLength := self fixedBlockSizeFor: lTree and: dTree.
	dynamicLength := self dynamicBlockSizeFor: lTree and: dTree 
							using: blTree and: blFreq.
	VerboseLevel > 1 ifTrue:[
		Transcript cr; show:'Block sizes (S/F/D):';
			space; print: storedLength // 8; 
			nextPut:$/; print: fixedLength // 8; 
			nextPut:$/; print: dynamicLength // 8; space; endEntry].

	"Check which method to use"
	method := self forcedMethod.
	method ifNil:[
		method := (storedLength < fixedLength and:[storedLength < dynamicLength]) 
			ifTrue:[#stored]
			ifFalse:[fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]]].
	(method == #stored and:[blockStart < 0]) ifTrue:[
		"Cannot use #stored if the block is not available"
		method := fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]].

	bitsSent := encoder bitPosition. "# of bits sent before this block"
	bitsRequired := nil.

	(method == #stored) ifTrue:[
		VerboseLevel > 0 ifTrue:[Transcript show:'S'].
		bitsRequired := storedLength.
		encoder nextBits: 3 put: StoredBlock << 1 + lastFlag.
		self sendStoredBlock].

	(method == #fixed) ifTrue:[
		VerboseLevel > 0 ifTrue:[Transcript show:'F'].
		bitsRequired := fixedLength.
		encoder nextBits: 3 put: FixedBlock << 1 + lastFlag.
		self sendFixedBlock].

	(method == #dynamic) ifTrue:[
		VerboseLevel > 0 ifTrue:[Transcript show:'D'].
		bitsRequired := dynamicLength.
		encoder nextBits: 3 put: DynamicBlock << 1 + lastFlag.
		self sendDynamicBlock: blTree 
			literalTree: lTree 
			distanceTree: dTree 
			bitLengths: blBits].

	bitsRequired = (encoder bitPosition - bitsSent)
		ifFalse:[self error:'Bits size mismatch'].

	lastBlock 
		ifTrue:[self release]
		ifFalse:[self initializeNewBlock].