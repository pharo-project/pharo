testing
isFaulty
	(self arguments anySatisfy: #isFaulty) ifTrue:[ ^true].
	(self pragmas anySatisfy: #isFaulty) ifTrue:[ ^true].
	^self body isFaulty