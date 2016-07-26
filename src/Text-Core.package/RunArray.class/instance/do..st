enumerating
do: aBlock
	"This is refined for speed"
	
	1 to: runs size do: [:i |
		| r v |
		v := values at: i.
		r := runs at: i.
		[( r := r - 1) >= 0]
			whileTrue: [aBlock value: v]].