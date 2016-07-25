column handling
headerBounds
	"return the bounds of each top header button (one per column)"

	| positions controlBounds currPos currLeft |
	controlBounds := OrderedCollection new.
	currPos := scroller left.
	(positions := self columnResizers asOrderedCollection collect: [ :r | r position ])
		ifNotEmpty: [ 
			| currRight |
			currPos := positions removeFirst x.
			currLeft := scroller left - scroller offset x.
			currRight := currPos.
			controlBounds add: (currLeft @ topHeader top corner: currRight @ topHeader bottom).
			[ positions notEmpty ]
				whileTrue: [ 
					currLeft := currPos + self resizerWidth.
					currPos := positions removeFirst x.
					currRight := currPos.
					controlBounds add: (currLeft @ topHeader top corner: currRight @ topHeader bottom) ] ].
	self columnResizers size < self columns size
		ifTrue: [ 
			currLeft := currPos + self resizerWidth.
			controlBounds add: ((currLeft min: scroller right) @ topHeader top corner: scroller right @ topHeader bottom) ].
	^ controlBounds