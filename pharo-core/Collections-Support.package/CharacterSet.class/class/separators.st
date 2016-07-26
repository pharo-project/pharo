instance creation
separators
	"return a set containing just the whitespace characters"

	| set |
	set := self empty.
	set addAll: Character separators.
	^set