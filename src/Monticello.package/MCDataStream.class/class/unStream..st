instance creation
unStream: aString

	^(self on: ((RWBinaryOrTextStream with: aString) reset; binary)) next