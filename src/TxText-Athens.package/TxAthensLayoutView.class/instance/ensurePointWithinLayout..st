geometry
ensurePointWithinLayout: aLayoutPoint
	"Note, the layout will not extend beyond the actual text"
	[ aLayoutPoint y > layout height ] whileTrue: [ 
		" try to extend layout until we cover given point"
		layout extendOneLineDown ifFalse: [  ^ self ]		
	].

	aLayoutPoint y < 0 ifTrue: [ | y h |
		y := aLayoutPoint y.
		h := layout height.
		layout extendOneLineUp ifFalse: [ ^ self ].
		y := y - (layout height - h).
	 ]