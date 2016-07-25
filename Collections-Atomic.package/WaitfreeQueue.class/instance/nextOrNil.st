accessing
nextOrNil
	"Fetch next item from queue, or nil if queue is either empty or in the middle of extraction by other process.
	If queue can contain a nil as element, use #nextIfNone: instead "
	^ self nextIfNone: nil