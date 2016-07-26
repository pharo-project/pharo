selection
buildSelectionBlocksFrom: topLeft to: bottomRight
	| viewedString primary topLeftBlk bottomRightBlk findReplaceIntervals secondarySelectionIntervals startIdx stopIdx |
	Display depth = 1
		ifTrue: [ ^ self ].
	Display depth = 2
		ifTrue: [ ^ self ].
	primary := selectionStart notNil
		ifTrue: [ selectionStart stringIndex to: selectionStop stringIndex - 1 ]
		ifFalse: [ 0 to: -1 ].
	topLeftBlk := self characterBlockAtPoint: topLeft.
	bottomRightBlk := self characterBlockAtPoint: bottomRight.
	startIdx := topLeftBlk stringIndex.
	stopIdx := bottomRightBlk stringIndex.
	viewedString := (self text copyFrom: startIdx to: stopIdx) asString.
	self theme currentSettings haveSecondarySelectionTextColor
		ifTrue: [ self text removeAttribute: TextSelectionColor secondarySelection ].
	self theme currentSettings haveFindReplaceSelectionTextColor
		ifTrue: [ self text removeAttribute: TextSelectionColor findReplaceSelection ].
	findReplaceIntervals := #().
	extraSelectionBlocks := Array
		streamContents: [ :strm | 
			findReplaceSelectionRegex
				ifNotNil: [ 
					findReplaceIntervals := findReplaceSelectionRegex matchingRangesIn: viewedString.
					findReplaceIntervals := (findReplaceIntervals collect: [ :r | r + topLeftBlk stringIndex - 1 ])
						reject: [ :r | primary size > 0 and: [ (r includes: primary first) or: [ primary includes: r first ] ] ].
					findReplaceIntervals
						do: [ :r | 
							self theme currentSettings haveFindReplaceSelectionTextColor
								ifTrue: [ self text addAttribute: TextSelectionColor findReplaceSelection from: r first to: r last ].
							strm
								nextPut:
									(ParagraphSelectionBlock
										first: (self characterBlockForIndex: r first)
										last: (self characterBlockForIndex: r last + 1)
										color: self findReplaceSelectionColor) ] ].
			secondarySelection
				ifNotNil: [ 
					secondarySelectionIntervals := (secondarySelection
						reject: [ :i | (findReplaceIntervals includes: i) or: [ i = primary or: [ i first > self text size ] ] ])
						collect: [ :i | i first to: (i last min: self text size) ].
					secondarySelectionIntervals
						do: [ :r | 
							self theme currentSettings haveSecondarySelectionTextColor
								ifTrue: [ self text addAttribute: TextSelectionColor secondarySelection from: r first to: r last ].
							strm
								nextPut:
									(ParagraphSelectionBlock
										first: (self characterBlockForIndex: r first)
										last: (self characterBlockForIndex: r last + 1)
										color: self secondarySelectionColor) ] ] ].
	findReplaceSelectionRegex := nil.
	secondarySelection := nil