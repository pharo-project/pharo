positioning
skipSeparators

	| state character |
	[ 
		state := converter saveStateOf: self.
		(character := self next) 
			ifNil: [ false ]
			ifNotNil: [ character isSeparator ] ] whileTrue.
	character ifNotNil: [ 
		converter restoreStateOf: self with: state ]