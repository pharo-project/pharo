initialize
postFlushIfNeeded
	"Force all pending primitives onscreen"
	workBuffer ifNil:[^self].
	(deferred not or:[postFlushNeeded]) ifTrue:[
		self copyBits.
		self release].