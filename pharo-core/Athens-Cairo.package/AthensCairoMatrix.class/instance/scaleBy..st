transformations
scaleBy: factor

	"if factor is number, do a uniform scale, 
	if not, then factor is assument to be an instance of Point containing non-uniform scale for each axis"
	factor isPoint 
		ifTrue: [ self scaleX: factor x asFloat Y: factor y asFloat ] 
		ifFalse: [ self scaleX: factor asFloat Y: factor asFloat ]
