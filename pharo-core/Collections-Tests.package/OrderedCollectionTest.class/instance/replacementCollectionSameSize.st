requirements
replacementCollectionSameSize
" return a collection of size (secondIndex - firstIndex + 1)"

| res |
res := OrderedCollection new.
1 to: (self secondIndex - self firstIndex + 1) do:
	[
	:i |
	res add: 99.
	].
	^res.