private
pastEndRead
	"A client has attempted to read beyond the read limit.
	Check in what state we currently are and perform
	the appropriate action"
	| blockType bp oldLimit |
	state = StateNoMoreData ifTrue:[^nil]. "Get out early if possible"
	"Check if we can move decoded data to front"
	self moveContentsToFront.
	"Check if we can fetch more source data"
	self moveSourceToFront.
	state = StateNewBlock ifTrue:[state := self getNextBlock].
	blockType := state bitShift: -1.
	bp := self bitPosition.
	oldLimit := readLimit.
	self perform: (BlockTypes at: blockType+1).
	"Note: if bit position hasn't advanced then nothing has been decoded."
	bp = self bitPosition 
		ifTrue:[^self primitiveFailed].
	"Update crc for the decoded contents"
	readLimit > oldLimit 
		ifTrue:[crc := self updateCrc: crc from: oldLimit+1 to: readLimit in: collection].
	state = StateNoMoreData ifTrue:[self verifyCrc].
	^self next