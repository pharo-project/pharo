match enumeration
matchesOnStream: aStream

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aStream
		do: [:match | result add: match].
	^result