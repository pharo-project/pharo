actions
addGroup
	"Add a new group to the configuration"

	| groupName |
	project
		ifNotNil: [ 
			groupName := UIManager default request: 'New group:'.
			groupName ifNotNil: [ project addDependency: (MTGroup new name: groupName) ].
			groupList roots: project groups ]