private
defaultLabel
	| stream |
	stream := String new writeStream.
	categories do: [:each | stream nextPutAll: each; nextPut: $ ].
	^stream contents