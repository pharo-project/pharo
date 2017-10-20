accessing
basename
	"Returns the base of the basename, 
		i.e. 
		/foo/gloops.taz basename is 'gloops.taz'
		/ basename is '/'"
	self size == 0
		"the root node"
		ifTrue: [ ^ '/'].
	^ self at: self size