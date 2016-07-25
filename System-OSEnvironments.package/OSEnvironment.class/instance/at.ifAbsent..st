accessing
at: aKey ifAbsent: aBlock
	^ (self getEnv: aKey) ifNil: aBlock