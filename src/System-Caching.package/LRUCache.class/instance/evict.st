private
evict
	| link value |
	link := lruList removeFirst.
	value := link value.
	weight remove: value value.
	keyIndex removeKey: value key