private
randomAsciiString: count
	^ String
		new: count
		streamContents: [ :stream | 
			count timesRepeat: [ stream nextPut: '0123456789ABCDEF' atRandom ] ]