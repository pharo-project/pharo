system compatibility
overriddenMethods
	^ Array streamContents: [:stream |
		self overriddenMethodsDo: [:each | stream nextPut: each]]
