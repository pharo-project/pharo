accessing
writingDeferrer

	^ writingDeferrer ifNil: [
		writingDeferrer := OmDeferrer 
			send: #flushEntryBuffer 
			to: self
			after: self class defaultWritingDeferDuration ]