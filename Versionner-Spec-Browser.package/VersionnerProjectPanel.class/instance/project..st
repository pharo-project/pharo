api
project: anMTProject
	project := anMTProject.
	project
		ifNil: [ 
			groupList roots: #().
			projectList roots: #() ]
		ifNotNil: [ 
			project announcer weak 
				when: MTProjectModelChanged
				send:  #refreshGroups
				to:  self.
			groupList roots: project groups.
			projectList roots: project requiredProjects ].
	packagesPanel project: anMTProject.
	addProjectButton enabled: project isNotNil.
	addGroupButton enabled: project isNotNil.
	packagesPanel addPackageButton enabled: project isNotNil.