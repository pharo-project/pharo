initialize
flush
	"Force all pending primitives onscreen"
	workBuffer ifNil:[^self].
	self copyBits.
	self release.