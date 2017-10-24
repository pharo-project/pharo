initialization
initializeWidgets
	self initializeGroupsWidget.
	self initializeProjectsWidget.
	
	packagesPanel := self instantiate: VersionnerProjectPackagesPanel.

	groupsLabel  := (self instantiate: LabelPresenter)
		label: 'Groups';
		yourself.
	
	projectsLabel :=  self instantiate: LabelPresenter.
	projectsLabel label: 'Dependent projects'.
