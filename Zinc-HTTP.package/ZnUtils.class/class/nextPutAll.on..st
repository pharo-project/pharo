streaming
nextPutAll: collection on: stream
	| totalWritten toWrite total |
	total := collection size.
	"Do not enter the loop for small sizes"
	total < self streamingBufferSize
		ifTrue: [ ^ stream nextPutAll: collection ].
	totalWritten := 0.
	[ totalWritten < total ] whileTrue: [ 
		toWrite := self streamingBufferSize min: (total - totalWritten).
		stream next: toWrite putAll: collection startingAt: totalWritten + 1.
		totalWritten := totalWritten + toWrite.
		self signalProgress: totalWritten total: total ]