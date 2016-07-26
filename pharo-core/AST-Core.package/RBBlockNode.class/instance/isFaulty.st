testing
isFaulty
	^(self arguments anySatisfy:  #isFaulty ) or: [ self body isFaulty]