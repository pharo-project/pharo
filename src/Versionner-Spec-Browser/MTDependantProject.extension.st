Extension { #name : #MTDependantProject }

{ #category : #'*Versionner-Spec-Browser' }
MTDependantProject >> editRepositories [
	| packageName repoManager |
	
	packageName := configurationClass package name.
	repoManager := VersionnerRepositoriesManager newWithPackage: packageName.
	repoManager
		selectedChangedBlock:  [:remote :selected |
			selected
				ifTrue: [ self repositories add: remote location ]
				ifFalse: [ self repositories remove: remote location ] ];
		open;
		yourself.
]

{ #category : #'*Versionner-Spec-Browser' }
MTDependantProject >> editVersion [
	| availableVersions  index |
	
	configurationClass ifNil: [ UIManager inform: 'Cannot find the configuration class for ' , name.
							^ self ].
	availableVersions := configurationClass project versions 
							select: [ :aVersion | aVersion blessing ~= #baseline ]
							thenCollect: [ :aVersion | aVersion versionString ].
	availableVersions := configurationClass project symbolicVersionSymbols , availableVersions.
	
	index := UIManager default 
		chooseFrom:  availableVersions
		title: 'Available versions in ', name.
	(index = 0)
		ifFalse: [ | versionString |
				versionString := availableVersions at: index.
				self version: (MTVersion fromVersionString:  versionString) ]
]

{ #category : #'*Versionner-Spec-Browser' }
MTDependantProject >> treeNodeClass [
	^ VSProjectLeafNode
]
