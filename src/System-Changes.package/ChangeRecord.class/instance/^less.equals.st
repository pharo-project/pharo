testing
<= anotherOne

	self stamp ifNil: [ ^ false ].
	anotherOne stamp ifNil: [ ^ true ].
	^ self timeStamp <= anotherOne timeStamp 