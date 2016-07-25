accessing-selection
highlightedTextAsStream
	"Answer a ReadStream on the text in the paragraph that is currently highlighted."

	| highlightedInterval |
	highlightedInterval := self getHighlightInterval.
	^ (self string copyFrom: highlightedInterval first to: highlightedInterval last) readStream