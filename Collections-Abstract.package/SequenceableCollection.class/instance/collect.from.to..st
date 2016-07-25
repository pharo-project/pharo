enumerating
collect: aBlock from: firstIndex to: lastIndex
	"Refer to the comment in Collection|collect:."

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self species new: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result