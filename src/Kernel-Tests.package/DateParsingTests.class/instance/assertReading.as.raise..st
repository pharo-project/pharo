asserting
assertReading: aString as: aPattern raise: anErrorClass

	self
		should: [ self reading: aPattern pattern: aString ]
		raise: anErrorClass
	