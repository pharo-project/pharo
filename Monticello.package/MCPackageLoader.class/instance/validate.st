private
validate
	self analyze.
	unloadableDefinitions isEmpty ifFalse: [self warnAboutDependencies].