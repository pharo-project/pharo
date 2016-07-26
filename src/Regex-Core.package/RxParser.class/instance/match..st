private
match: aCharacter
	"<aCharacter> MUST match the current lookeahead.
	If this is the case, advance the input. Otherwise, blow up."

	aCharacter ~= lookahead 
		ifTrue: [^self signalParseError].	"does not return"
	self next