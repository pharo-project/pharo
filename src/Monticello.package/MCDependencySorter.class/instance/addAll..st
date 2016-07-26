building
addAll: aCollection
	aCollection asArray sort do: [:ea | self add: ea]