ancestry
ancestorsDoWhileTrue: aBlock
	self ancestors do:
		[:ea |
		(aBlock value: ea) ifTrue: 
			[ea ancestorsDoWhileTrue: aBlock]]