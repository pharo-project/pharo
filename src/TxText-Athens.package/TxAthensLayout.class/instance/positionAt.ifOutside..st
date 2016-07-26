geometry
positionAt: aTextPosition ifOutside: aBlock
	(self interval contains: aTextPosition)
		ifTrue:[ ^ TxAthensLayoutPosition textPosition: aTextPosition layout: self ] 
		ifFalse: [  ^ aBlock value ].
		 
