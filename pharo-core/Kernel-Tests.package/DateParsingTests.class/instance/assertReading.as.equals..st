asserting
assertReading: aString as: aPattern equals: aDate
	self assert: (self reading: aPattern pattern: aString) equals: aDate