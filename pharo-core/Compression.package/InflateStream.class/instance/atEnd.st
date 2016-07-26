testing
atEnd
	"Note: It is possible that we have a few bits left,
	representing just the EOB marker. To check for
	this we must force decompression of the next
	block if at end of data."
	super atEnd ifFalse:[^false]. "Primitive test"
	(position >= readLimit and:[state = StateNoMoreData]) ifTrue:[^true].
	"Force decompression, by calling #next. Since #moveContentsToFront
	will never move data to the beginning of the buffer it is safe to
	skip back the read position afterwards"
	self next ifNil: [^true].
	position := position - 1.
	^false