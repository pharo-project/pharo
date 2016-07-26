private
randomUnicodeString: count
	^ String
		new: count
		streamContents: [ :stream | 
			"This is about 6% non-ASCII characters, still a ByteString"
			count timesRepeat: [ stream nextPut: '0123456789ABCDEF-élève en Français-0123456789ABCDEF' atRandom ] ]