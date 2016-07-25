accessing
at: aKey ifPresent: aBlock
	^ aBlock value: (self at: aKey ifAbsent: [ ^ nil ])