accessing
skipAnySubstring: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."

	| any this ind ii |
	ii := start-1.
	[(ii := ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		any := false.
		delimiters do: [:delim |
			delim isCharacter 
				ifTrue: [(self at: ii) == delim ifTrue: [any := true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: "Here's where the one-off error was."
						[ind := 0.
						this := true.
						delim do: [:dd | 
							dd == (self at: ii+ind) ifFalse: [this := false].
							ind := ind + 1].
						this ifTrue: [ii := ii + delim size - 1.  any := true]]
							ifTrue: [any := false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1