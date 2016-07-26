tests-strings
testString

	self assertSerializationEqualityOf: 'testString'.
	self assertSerializationEqualityOf:  'Hi, I''m String-object'.
	self assertSerializationEqualityOf:  String new.
	self assertSerializationEqualityOf:  (0 to: 255) asByteArray asString.
	
