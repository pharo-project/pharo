instance creation
bytes: aByteArray
	| answer |
	answer := self basicNew
		bytes: aByteArray;
		yourself.
	^answer