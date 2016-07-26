initialize-release
initializeStream: aWriteStream
	stream := aWriteStream.
	"This is ugly, but it is an optimization for #flushBuffer"
	streamRespondsToNextPutAllStartingAt := (stream respondsTo: #next:putAll:startingAt:).