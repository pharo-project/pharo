API
label: aString

	self label isEmpty ifTrue: [ self forceRefreshOnNextChange ].
	self labelMorph contents: aString.
	self changed.