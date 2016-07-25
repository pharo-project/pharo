match enumeration
matchesOnStream: aStream collect: aBlock

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aStream
		do: [:match | result add: (aBlock value: match)].
	^result