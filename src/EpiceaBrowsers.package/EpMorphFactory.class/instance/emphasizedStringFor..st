strings
emphasizedStringFor: aString

	| morph |
	morph := aString asMorph
		emphasis: 1;
		yourself.

	"Hack: emphazised string has 3 blank pixels at the end"
	morph width: (morph width - 3). "pffff"
		
	^ morph