tests-strings
testSymbol

	self assertSerializationIdentityOf: #testSymbol.
	self assertSerializationIdentityOf: #with:with:with:with:.
	self assertSerializationIdentityOf: #'hello there'.