accessing
minWidth
	"Fixed to work such that guessed width is unnecessary in
	#adjustSubmorphPositions."
	| gap |
	
	gap := container ifNil: [ 0 ] ifNotNil: [ self indentGap ].
	^ gap  max: super minWidth