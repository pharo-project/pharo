initialization
initializeWidgets
	self initializeGroupsWidget.
	self initializeProjectsWidget.
	
	packagesPanel := self instantiate: VersionnerProjectPackagesPanel.

	groupsLabel  := (self instantiate: LabelModel)
		label: 'Groups';
		yourself.
	
	projectsLabel :=  self instantiate: LabelModel.
	projectsLabel label: 'Dependent projects'.
