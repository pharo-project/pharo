initialization
initialize
	super initialize.
	order := self class order.
	showOnlyRepositoriesFromWorkingCopy := true.
	self registerToAnnouncer.
	workingCopyPattern := ''.
	repositoryPattern := ''