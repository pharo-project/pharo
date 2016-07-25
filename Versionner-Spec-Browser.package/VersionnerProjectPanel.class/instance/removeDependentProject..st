actions
removeDependentProject: aMTDependantProject 
	aMTDependantProject remove.
	projectList roots: project requiredProjects.