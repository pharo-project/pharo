private
at: index setRunOffsetAndValue: aBlock 
	"Supply all run information to aBlock."
	"Tolerates index=0 and index=size+1 for copyReplace: "
	| run limit offset |
	limit := runs size.
	(lastIndex == nil or: [index < lastIndex])
		ifTrue: "cache not loaded, or beyond index - start over"
			[run := 1.
			offset := index-1]
		ifFalse: "cache loaded and before index - start at cache"
			[run := lastRun.
			offset := lastOffset + (index-lastIndex)].
	[run <= limit and: [offset >= (runs at: run)]]
		whileTrue: 
			[offset := offset - (runs at: run).
			run := run + 1].
	lastIndex := index. "Load cache for next access"
	lastRun := run.
	lastOffset := offset.
	run > limit
		ifTrue: 
			["adjustment for size+1"
			run := run - 1.
			offset := offset + (runs at: run)].
	^aBlock
		value: run "an index into runs and values"
		value: offset "zero-based offset from beginning of this run"
		value: (values at: run) "value for this run"