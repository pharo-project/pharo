private
defaultModel
	^ (Smalltalk globals at: #MCMultiPackageLoader ifAbsent: [ MCPackageLoader ]) new