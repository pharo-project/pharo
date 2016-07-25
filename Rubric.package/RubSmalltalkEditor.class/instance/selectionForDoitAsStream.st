do-its
selectionForDoitAsStream
	"Answer a ReadStream on the text in the paragraph that is currently
	selected. "
	^ ReadWriteStream
		on: self string
		from: self startIndex 
		to: self stopIndex - 1