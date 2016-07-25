match enumeration
matchingRangesIn: aString
	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of ranges of each match (index of first character to: index of last character)."

	| result |
	result := OrderedCollection new.
	self
		matchesIn: aString 
		do: [:match | result add: (self position - match size + 1 to: self position)].
	^result