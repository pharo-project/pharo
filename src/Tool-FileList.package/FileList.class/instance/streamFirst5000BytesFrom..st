private
streamFirst5000BytesFrom: aBinaryReadStream
	^ [ self streamFirst5000BytesWithEncodingFrom: aBinaryReadStream ]
		on: Error
		do: [ :ex |(aBinaryReadStream next: 5000) asString ]