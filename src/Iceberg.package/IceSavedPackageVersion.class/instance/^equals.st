comparing
= another
	self species = another species ifFalse: [ ^ false ].
	
	"If I both load commits are equal, we are the same version."
	self commit = another commit ifTrue: [ ^ true ]. 

	"If not, look for actual origination commits and compare them."
	^ self originatingCommit = another originatingCommit.