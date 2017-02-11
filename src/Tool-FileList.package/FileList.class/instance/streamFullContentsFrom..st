private
streamFullContentsFrom: aBinaryReadStream
	^ [ self streamFullContentsWithEncodingFrom: aBinaryReadStream ]
		on: Error
		do: [ :ex | aBinaryReadStream contents asString ]