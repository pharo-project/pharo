tests-numbers
testSmallInteger

	self assertSerializationIdentityOf: -1212.
	self assertSerializationIdentityOf: 7.
	self assertSerializationIdentityOf: 0.
	self assertSerializationIdentityOf: 1111.
	self assertSerializationIdentityOf: SmallInteger one.

