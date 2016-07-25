accessing
searchFor: aStringOrText
	pattern := aStringOrText asString trim.
	^ self realSearch