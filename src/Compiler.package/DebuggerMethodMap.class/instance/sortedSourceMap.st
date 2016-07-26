source mapping
sortedSourceMap
	"Answer a sorted collection of associations, pcRangeStart -> pcRangeInterval "
	^ sortedSourceMap
		ifNil: [sortedSourceMap := self abstractSourceMap associations sorted]