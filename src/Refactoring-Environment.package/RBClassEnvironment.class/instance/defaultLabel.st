private
defaultLabel
	| stream |
	stream := String new writeStream.
	classes do: 
			[:each | 
			stream nextPutAll: each;
				nextPut: $ ].
	^stream contents