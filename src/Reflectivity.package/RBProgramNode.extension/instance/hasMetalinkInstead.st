*Reflectivity
hasMetalinkInstead
		self hasMetalink ifFalse: [^false].
		^self insteadLinks isNotEmpty