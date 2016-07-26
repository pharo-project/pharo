actions
addDependentProject
	"Add a new dependent project to the configuration"

	| requiredProjectNames dependentProject searchUI |
	project
		ifNotNil: [
			requiredProjectNames := project requiredProjects collect: [ :reqProject | reqProject name ].
			searchUI :=  SearchFacade projectWithConfigurationSearchRejectAll: [ :item | 
				(requiredProjectNames includes: item name configurationBaseName) 
					or: [ project name = item name configurationBaseName ] ].
			dependentProject := searchUI chooseFromOwner: self window.
			dependentProject ifNotNil: [ project addDependency: (MTDependantProject newWithWorkingCopy: dependentProject) ].
			projectList roots: project requiredProjects ]