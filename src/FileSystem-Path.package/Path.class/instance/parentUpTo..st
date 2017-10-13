navigating
parentUpTo: aParentDirName
	"Answers the path of the parent dir with name aParentDirName or root if not found."

	self withParents
		reverseDo: [ :dir | 
			dir basename = aParentDirName
				ifTrue: [ ^ dir ] ].
	^ Path root