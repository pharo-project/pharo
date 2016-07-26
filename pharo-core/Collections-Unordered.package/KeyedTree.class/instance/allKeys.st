accessing
allKeys
	"Answer an ordered collection of the keys of the receiver and any subtrees.
		Please no circular references!"

	|answer|
	answer := OrderedCollection new.
	answer addAll: self keys.
	self subtrees do: [:t |
		answer addAll: t allKeys].
	^answer