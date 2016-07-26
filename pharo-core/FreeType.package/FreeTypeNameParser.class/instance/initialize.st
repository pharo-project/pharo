initialization
initialize
	super initialize.
	delimiters := ',.-_'.
	Character separators do:[:c | delimiters := delimiters , c asString].
