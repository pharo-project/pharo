load and saving
save: aDictionary into: aWriteStream
	aWriteStream binary.
	[ FLSerializer serialize: aDictionary on: aWriteStream ]
		on: Error
		do: [ :anError | "ignore" ]