private
skipWhitespace: stream
	[ stream atEnd not and: [ (self isWhitespaceCharacter: stream peek) ] ] 
		whileTrue: [ stream next ]