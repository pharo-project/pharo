accessing
ownFieldSize
	|result|
	result := 0.
	self do: [ :slot | result := result + slot size ].
	^ result