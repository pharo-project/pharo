protocol
maxSize

	^ maxSize ifNil: [ maxSize := self class defaultMaxSize ].