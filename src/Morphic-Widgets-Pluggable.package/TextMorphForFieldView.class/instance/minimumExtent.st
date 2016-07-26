geometry
minimumExtent
	"Use the actual paragraph line to take font changes into account."
	
	| minExt |
	textStyle ifNil: [^ 9@16].
	borderWidth ifNil: [^ 9@16].
	minExt := (9@(self paragraph lines first lineHeight ceiling)) + (borderWidth*2).
	^ ((0@0 extent: minExt) expandBy: margins) extent