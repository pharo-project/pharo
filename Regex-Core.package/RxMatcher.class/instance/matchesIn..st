match enumeration
matchesIn: aString
	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of all matches (substrings)."

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aString readStream
		do: [:match | result add: match].
	^result