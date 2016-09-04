accessing
skipSeparatorsAndPeekNext
	"Same as #skipSeparators, but returns the next character after the separators if such exists."

	| state character |
	[ 
		state := converter saveStateOf: self.
		(character := self next) 
			ifNil: [ false ]
			ifNotNil: [ character isSeparator ] ] whileTrue.
	character ifNotNil: [ 
		converter restoreStateOf: self with: state.
		^character ].
	