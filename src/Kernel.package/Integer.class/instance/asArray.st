converting-arrays
asArray

	| stream |
	stream := Array new writeStream.
	self digitLength to: 1 by: -1 do: [:digitIndex |
		stream nextPut: (self digitAt: digitIndex)].
	^ stream contents
