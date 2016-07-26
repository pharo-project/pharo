testing
isSeparator
	self asInteger = 32
		ifTrue: [ ^ true ].
	self asInteger = 13
		ifTrue: [ ^ true ].
	self asInteger = 9
		ifTrue: [ ^ true ].
	self asInteger = 10
		ifTrue: [ ^ true ].
	self asInteger = 12
		ifTrue: [ ^ true ].
	^ false