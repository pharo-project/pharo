accessing
titleText: anObject
	titleText := anObject.
	
	self titleBrick ifNotNil: [ self titleBrick text: self titleText ]