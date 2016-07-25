enumerating
reverseDo: aBlock
	"This is refined for speed"
	
	| i |
	i := runs size.
	[i > 0]
		whileTrue: 
			[ | r v |
			v := values at: i.
			r := runs at: i.
			i := i - 1.
			[( r := r - 1) >= 0]
				whileTrue: [aBlock value: v]].