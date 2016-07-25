initialization
newCaseInsensitiveOrder
	"map char and char asLower (Lowercase Latin1 stays in the Latin1 range, uppercase not.)"
	| newCollection |
	newCollection := AsciiOrder copy.
    (0 to: 255) do:[ :v |
            | char lower |
            char := v asCharacter.
            lower := char asLowercase.
            newCollection at: lower asciiValue + 1 put: (newCollection at: char asciiValue + 1) ].
	^ newCollection 