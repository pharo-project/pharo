accessing
maximumUsableAreaInWorld: aWorldOrNil

	| allowedArea |
	allowedArea := Display usableArea.
	aWorldOrNil ifNotNil: [
		allowedArea := allowedArea intersect: aWorldOrNil visibleClearArea ifNone: [ allowedArea ]].
	^allowedArea