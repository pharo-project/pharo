new selection
nextTokenFrom: start direction: dir
	"simple token-finder for compiler automated corrections"
	| loc str |
	loc := start + dir.
	str := self string.
	[(loc between: 1 and: str size) and: [(str at: loc) isSeparator]]
		whileTrue: [loc := loc + dir].
	^ loc