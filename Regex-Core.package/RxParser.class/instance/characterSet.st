recursive descent
characterSet
	"Match a range of characters: something between `[' and `]'.
	Opening bracked has already been seen, and closing should
	not be consumed as well. Set spec is as usual for
	sets in regexes."

	| spec errorMessage |
	errorMessage := ' no terminating "]"'.
	spec := self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage.
	(spec isEmpty 
	or: [spec = '^']) 
		ifTrue: [
			"This ']' was literal." 
			self next.
			spec := spec, ']', (self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage)].
	^self characterSetFrom: spec