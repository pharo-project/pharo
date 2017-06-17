descriptions
description
	"Use local directory as description, as Monticello uses this to determine if two repositories are the same. This allows for having multiple local clones of the same remote repositories, which in turn could allow for using standard Pharo tools, for example to compare versions."
	^ self basicCodeDirectory pathString, ' (', self class description, ')'
