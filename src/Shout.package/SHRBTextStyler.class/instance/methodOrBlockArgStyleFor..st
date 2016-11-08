private
methodOrBlockArgStyleFor: anArgumentNode
	^ anArgumentNode isBlockVar
		ifTrue: [ #blockArg ]
		ifFalse: [ #methodArg ]