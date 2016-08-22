private-scanning
scanUnknownCharacter
	| errorToken |
	errorToken := self scanError: 'Unknown character'.
	"advance"
	self step.
	^ errorToken