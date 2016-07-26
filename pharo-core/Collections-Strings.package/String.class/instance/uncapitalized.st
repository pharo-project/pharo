converting
uncapitalized
	"Return a copy with the first letter downShifted (in lower case)"
	
	| answer |
	self ifEmpty: [ ^ self copy ].
	answer := self copy.
	answer at: 1 put: answer first asLowercase.
	^ answer