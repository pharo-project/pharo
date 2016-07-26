accessing
basename: newBasename
	"change the basename"
	self size == 0
		"the root node"
		ifTrue: [ ^ Error signal: '0 length Path, cannot change basename'].
	self at: self size put: newBasename