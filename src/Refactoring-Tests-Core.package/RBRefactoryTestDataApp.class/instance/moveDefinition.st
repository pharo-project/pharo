test
moveDefinition
	| temp |
	^(self collect: 
			[:each | 
			temp := each printString.
			temp , temp]) 
		select: 
			[:each | 
			temp := each size.
			temp odd]