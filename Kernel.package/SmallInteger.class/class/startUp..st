system startup
startUp: isImageStarting
	"The image is either being newly started (isImageStarting is true), or it's just been snapshotted.
	 If this has just been a snapshot, skip all the startup stuff."
	| next val |
	isImageStarting ifFalse: [^self].
	val := -32768. "Assume at least 16 bits"
	[next := val + val.
	next class == self] whileTrue:
		[val := next].
	minVal := val.
	maxVal := -1 - val