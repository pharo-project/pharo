private
flushReadBuffer

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| currentPosition |
			currentPosition := self position.
			position := readLimit := 0.
			self primSetPosition: fileID to: currentPosition ] ]