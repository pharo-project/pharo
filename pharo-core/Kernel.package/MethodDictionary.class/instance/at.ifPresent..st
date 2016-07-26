accessing
at: key ifPresent: aBlock

	^(array at: (self findElementOrNil: key))
		ifNotNil: [ :value | aBlock cull: value ]