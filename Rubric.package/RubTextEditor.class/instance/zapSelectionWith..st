accessing-selection
zapSelectionWith: aText
	| start stop |
	self canChangeText
		ifFalse: [ ^ self ].
	start := self startIndex.
	stop := self stopIndex.
	(aText isEmpty and: [ stop > start ])
		ifTrue: [ 
			"If deleting, then set emphasisHere from 1st character of the deletion"
			self editingState emphasisHere: ((self text attributesAt: start) select: [ :att | att mayBeExtended ]) ].
	(start = stop and: [ aText size = 0 ])
		ifFalse: [ 
			textArea privateReplaceFrom: start to: stop - 1 with: aText asText.
			"We ask to editingSate directly instead to the morph "	
			"to avoid unwanted SelectionChanged announce."	
			"It will be send later while unselecting (with #unselect send)"
			self markIndex: start pointIndex: start + aText size.
			self editingState previousInterval: self selectionInterval ] 