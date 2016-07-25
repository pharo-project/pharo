private
defaultLabel
	| stream |
	stream := String new writeStream.
	keywords do: [ :each | stream nextPut: $<; nextPutAll: each; nextPut: $>; nextPut: $ ].
	^ stream contents