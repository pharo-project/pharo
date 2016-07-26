accessing
startPosition: aTxTextPosition 
	
	interval ifNotNil: [  self error: 'start position can be set only once' ].

	interval := (aTxTextPosition to: aTxTextPosition).
	"set initial height and ascent"
	self accumulateSpanFontMetrics: aTxTextPosition span.
			
