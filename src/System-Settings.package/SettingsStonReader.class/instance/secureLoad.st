loading
secureLoad
	[ self load ] on: STONReaderError do: [ :ex | "ignore" ].
	^ self storedSettings ifNil: [ #() ].