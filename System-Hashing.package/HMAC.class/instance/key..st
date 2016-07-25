accessing
key: aByteArray 
	key := aByteArray.
	key size > hash blockSize ifTrue: [ key := hash hashMessage: key ].
	key size < hash blockSize ifTrue: [ key := key , (ByteArray new: hash blockSize - key size) ]