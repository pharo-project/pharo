accessing
findDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | 
			delim = (self at: i) 
				ifTrue: [^ i]]].
	^ self size + 1