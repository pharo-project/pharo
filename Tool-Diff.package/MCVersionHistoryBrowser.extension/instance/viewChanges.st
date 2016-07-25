*Tool-Diff
viewChanges
	"View the changes between a prior version and this version."
	
	self
		viewChanges: (self baseSnapshot patchRelativeToBase: self selectedSnapshot)
		from:  self selectedInfo name
		to: ancestry name