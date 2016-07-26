*Reflectivity
allAfterAreWeak
	self hasMetalink ifFalse: [^false].
	^self afterLinks allSatisfy: [ :link | link hasOption: #optionWeakAfter ].