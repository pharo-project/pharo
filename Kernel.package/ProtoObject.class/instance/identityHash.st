comparing
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 This method must not be overridden, except by SmallInteger.  As of
	 2014, the 32-bit Spur VM has 22 bits of hash and 31-bit SmallIntegers
	 (30 bits + 1 sign bit).  Shifting by 8 will not create large integers.
	
	 Do not override."

	^self basicIdentityHash bitShift: 8