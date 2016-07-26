copying
canProceedAfter: failureReason
	"Check if we can proceed after the failureReason indicated."
	| newBuffer |
	failureReason = GErrorNeedFlush ifTrue:[
		"Need to flush engine before proceeding"
		self copyBits.
		self reset.
		^true].
	failureReason = GErrorNoMoreSpace ifTrue:[
		"Work buffer is too small"
		newBuffer := workBuffer species new: workBuffer size * 2.
		self primCopyBufferFrom: workBuffer to: newBuffer.
		workBuffer := newBuffer.
		^true].
	"Not handled"
	^false