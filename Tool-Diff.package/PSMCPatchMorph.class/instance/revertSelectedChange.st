private
revertSelectedChange
	"Revert the selected change"
	| change |
	change := self selectedChange.
	change ifNil: [ ^ self ].
	self revertChange: change.
	self model remove: change.
	self removeSelection.
