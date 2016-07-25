private
skipWhiteSpace: stream
	[ stream atEnd or: [ (self isLegalCharacter: stream peek) ] ] 
		whileFalse: [ stream next ]