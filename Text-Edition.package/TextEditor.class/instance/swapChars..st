editing keys
swapChars: aKeyboardEvent
	"Triggered byCmd-Y;.  Swap two characters, either those straddling the insertion point, or the two that comprise the selection.  Suggested by Ted Kaehler.  "

	| currentSelection aString chars |
	(chars := self selection) size = 0
		ifTrue:
			[currentSelection := self pointIndex.
			self selectMark: currentSelection - 1 point: currentSelection]
		ifFalse:
			[chars size = 2
				ifFalse:
					[morph flash. ^ true]
				ifTrue:
					[currentSelection := self pointIndex - 1]].
	aString := self selection string.
	self replaceSelectionWith: (Text string: aString reversed attributes: self emphasisHere).
	self selectAt: currentSelection + 1.
	^ true