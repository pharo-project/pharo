accessing
endPosition: aTxTextPosition 
	interval := interval 
		ifNil: [ aTxTextPosition to: aTxTextPosition ]
		ifNotNil: [ interval start to: aTxTextPosition ]