accessing
commandForPosition: aTextPosition ifOutside: aBlock
	"Answer the display command which correspond to given text position in line,
	or evaluate block if receiver does not contains given position"
	
	(interval contains: aTextPosition) ifTrue: [ 
			commands
				detect: [ :each | each contains: aTextPosition ]
				ifFound: [ :each | ^ each ] ].

	^ aBlock value