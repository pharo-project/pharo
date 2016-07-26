accessing
setTitle: aString

	self titleBrick text: aString.
	
	titleBrick height: (
		(aString isEmpty | aString isNil)
			ifFalse: [ 30 ]
			ifTrue: [ 0 ])