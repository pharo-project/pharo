accessing
maxListSize

	^maxListSize ifNil: [maxListSize := self defaultMaxListSize]