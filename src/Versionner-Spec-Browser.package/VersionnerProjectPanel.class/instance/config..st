api
config: anMBConfigurationInfo
	| allGroups |
	config := anMBConfigurationInfo.

	"allGroups := config groups."
	"(allGroups includes: 'All') ifFalse: [ allGroups addFirst: 'All' ] ."
"	self groups roots: allGroups."
	
	"self packagesPanel packages: config packages."
	"self projects: config projects".
	
	project :=  MCModel2MTModelVisitor new visitConfiguration: anMBConfigurationInfo.
	groupList roots: project groups.
	projectList items: project requiredProjects.
	packagesPanel project: project.