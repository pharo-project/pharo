accessing
titleText: aText
	titleText := aText.
	
	self titleBrick ifNotNil: [ self titleBrick text: self titleText ]