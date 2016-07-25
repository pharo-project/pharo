initialization
initialize 
	super initialize.
	commitStrategy := VSCommitStrategy new.
	self collectCommitActions