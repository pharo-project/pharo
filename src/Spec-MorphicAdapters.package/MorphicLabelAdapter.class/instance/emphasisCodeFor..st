spec protocol
emphasisCodeFor: anEmphasis
	^ anEmphasis
		inject: 0
		into: [ :sum :next | sum + (self emphasisOptions at: next) ]