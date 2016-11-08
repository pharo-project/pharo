private
methodOrBlockTempStyleFor: aTemporaryNode
	^ aTemporaryNode isBlockVar
		ifTrue: [ #blockTempVar ]
		ifFalse: [ #tempVar ]