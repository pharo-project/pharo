accessing
nextLine
	"Answer next line (may be empty) without line end delimiters, or nil if at end.
	Let the stream positioned after the line delimiter(s).
	Handle a zoo of line delimiters CR, LF, or CR-LF pair"

	self atEnd ifTrue: [^nil].
	^self upToAnyOf: CharacterSet crlf do: [:char | char = Character cr ifTrue: [self peekFor: Character lf]]