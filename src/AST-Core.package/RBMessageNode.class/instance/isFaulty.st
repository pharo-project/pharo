testing
isFaulty
	^self receiver isFaulty or: [self arguments anySatisfy: #isFaulty]