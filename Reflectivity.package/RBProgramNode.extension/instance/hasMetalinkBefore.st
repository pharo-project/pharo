*Reflectivity
hasMetalinkBefore
	self hasMetalink ifFalse: [^false].
	^self beforeLinks isNotEmpty