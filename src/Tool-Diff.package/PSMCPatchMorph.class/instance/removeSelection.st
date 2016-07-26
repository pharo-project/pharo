private
removeSelection
	"unselect current selection"
	self selectedChangeWrapper: nil.
	self changed: #changes.
	self updateSource