match enumeration
matchesIn: aString do: aBlock
	"Search aString repeatedly for the matches of the receiver.
	Evaluate aBlock for each match passing the matched substring
	as the argument."

	self
		matchesOnStream: aString readStream
		do: aBlock