printing
storeOn: aStream

	aStream << self class name << ' name: '<< self name printString <<' code: ' << code printString.
	self runOnce ifTrue: [ aStream << ' runOnce: true']