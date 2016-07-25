*Reflectivity
hasMetalinkAfter
	self hasMetalink ifFalse: [^false].
	^self afterLinks isNotEmpty