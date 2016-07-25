I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger.
	
Integer division consists of:
	/	exact division, answers a fraction if result is not a whole integer
	//	answers an Integer, rounded towards negative infinity
	\\	is modulo rounded towards negative infinity
	quo: truncated division, rounded towards zero