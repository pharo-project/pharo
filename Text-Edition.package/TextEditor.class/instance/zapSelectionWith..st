mvc compatibility
zapSelectionWith: aText
	| start stop |
	morph canChangeText ifFalse: [^ self].
	start := self startIndex.
	stop := self stopIndex.
	(aText isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		self editingState emphasisHere: ((self text attributesAt: start) select: [:att | att mayBeExtended])].
	(start = stop and: [ aText size = 0 ]) ifFalse: [
		self paragraph replaceFrom: start to: stop - 1 with: aText.
		self markIndex: start; pointIndex: start + aText size.
		self editingState previousInterval: self selectionInterval selection: self selection.
		self userHasEdited  " -- note text now dirty" ].
