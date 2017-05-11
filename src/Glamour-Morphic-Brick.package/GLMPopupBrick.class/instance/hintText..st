accessing
hintText: aBrick
	hintText := aBrick.

	self hintBrick ifNotNil: [ self hintBrick text: self hintText ]