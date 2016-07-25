point functions
octantOf: otherPoint 
	"Return 1..8 indicating relative direction to otherPoint.  
	1=ESE, 2=SSE, ... etc. clockwise to 8=ENE"

	| quad moreHoriz |
	(x = otherPoint x and: [ y > otherPoint y ]) ifTrue: [ ^ 6 ].
	"special case"
	(y = otherPoint y and: [ x < otherPoint x ]) ifTrue: [ ^ 8 ].
	quad := self quadrantOf: otherPoint.
	moreHoriz := (x - otherPoint x) abs >= (y - otherPoint y) abs.
	^ (quad even eqv: moreHoriz) 
		ifTrue: [ quad * 2 ]
		ifFalse: [ quad * 2 - 1 ]