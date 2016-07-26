current selection
reverseSelection
	"Reverse the valence of the current selection highlighting."
	selectionShowing := selectionShowing not.
	self paragraph reverseFrom: self editingState pointBlock to: self editingState markBlock