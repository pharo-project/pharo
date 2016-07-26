font creation
fromStrike: fileName 
	"Read a font from disk in the old ST-80 'strike' format.
	Note: this is an old format; use strike2 format instead"

	^self new newFromStrike: fileName