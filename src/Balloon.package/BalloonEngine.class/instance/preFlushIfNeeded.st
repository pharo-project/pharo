initialize
preFlushIfNeeded
	"Force all pending primitives onscreen"
	workBuffer ifNil:[^self].
	self primFlushNeeded ifTrue:[
		self copyBits.
		self reset].