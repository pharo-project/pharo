converting
asHex
	^ self class new: self size * 4 streamContents: [ :stream |
		self do: [ :ch | stream nextPutAll: ch hex ]]