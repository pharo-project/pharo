actions
changes
	"Open a patch morph for the changes."
	
	self
		viewChanges: self version changes
		from: self version workingCopy description
		to: self version info name