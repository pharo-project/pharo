splitjoin
join: aCollection
	"NB: this implementation only works for Array, since WriteStreams only work for Arrays and Strings. (!)
	Overridden in OrderedCollection and SortedCollection."
	^ self class
		streamContents: [:stream | aCollection
				do: [:each | each joinTo: stream]
				separatedBy: [stream nextPutAll: self]]