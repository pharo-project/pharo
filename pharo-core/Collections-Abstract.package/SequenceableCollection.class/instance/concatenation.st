converting
concatenation
	|result index|

	result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
	index := 0.
	self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
	^result