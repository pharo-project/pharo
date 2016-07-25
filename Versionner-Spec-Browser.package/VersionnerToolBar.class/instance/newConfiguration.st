actions
newConfiguration
	| name selection |
	name := UIManager default request: 'Name of the new project'.
	name isEmptyOrNil ifFalse: [ 
		Cursor execute showWhile:  [ | project |
			project := MTProject
				newNamed: name configurationBaseName
				withInitialVersion: '0.1'
				inRepository: nil.
			MCWorkingCopy forPackage: (MCPackage new name: project configurationClass name).
			selection :=  owner configurationList listItems 
					detect: [ :item | item name configurationBaseName = name configurationBaseName].
			owner configurationList setSelectedItem: selection.
			owner browseConfiguration: selection ]  ]