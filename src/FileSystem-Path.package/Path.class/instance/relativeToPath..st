navigating
relativeToPath: aPath
	"Return the receiver as relative to the argument aPath"

	"(Path / 'griffle' / 'plonk' / 'nurp') 
		relativeToPath: (Path / 'griffle') 
			returns  plonk/nurp"

	| prefix relative |
	prefix := self lengthOfStemWith: aPath.
	relative := RelativePath parents: aPath size - prefix.
	prefix + 1 to: self size do: [ :i | relative := relative / (self at: i) ].
	^ relative