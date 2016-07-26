find-replace
findAndSelect: aRegex startingAt: anIndex searchBackwards: searchBackwards
	| where |
	self
		handleEdit: [
			where := self editor
				findAndSelect: aRegex
				startingAt: anIndex
				searchBackwards: searchBackwards].
	editView scrollSelectionIntoView.
	^ where