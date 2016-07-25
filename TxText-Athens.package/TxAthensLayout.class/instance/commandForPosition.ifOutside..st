accessing
commandForPosition: aTextPosition ifOutside: aBlock
	"Answer the display command which correspond to given text position in layout,
	or evaluate block if layout does not contains given position"
	
	lines do: [:line |
		(line interval contains: aTextPosition) ifTrue: [ 
			^ line commandForPosition: aTextPosition ifOutside: aBlock ]
	].

	^ aBlock value