accessing
hintText: anObject
	hintText := anObject.
	
	self hintBrick ifNotNil: [ self hintBrick text: self hintText ]