public query
defaultSystemConverter
	SystemConverter
		ifNil: [ SystemConverter := self currentPlatform class systemConverterClass new ].
	^ SystemConverter