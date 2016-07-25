private
defaultLabel
	| stream |
	stream := String new writeStream.
	stream nextPutAll: class name;
		nextPut: $>.
	protocols do: 
			[:each | 
			stream nextPutAll: each;
				nextPut: $ ].
	^stream contents