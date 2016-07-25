private
clearVersionSpec: versionSpec fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages
	versionSpec
		specListProjectDo: [ :spec | 
			updateProjects
				ifTrue: [ 
					| fullProjectReferenceSpec fullProjectSpec |
					(fullProjectReferenceSpec := fullVersionSpec packageNamed: spec name) ~~ nil
						ifTrue: [ 
							fullProjectSpec := fullProjectReferenceSpec projectReference.
							fullProjectSpec versionString == nil
								ifTrue: [ spec projectReference versionString: '' ] ] ] ]
		packageDo: [ :spec | 
			updatePackages
				ifTrue: [ 
					spec getFile ~~ nil
						ifTrue: [ spec file: '' ] ] ]
		groupDo: [ :ignored |  ]