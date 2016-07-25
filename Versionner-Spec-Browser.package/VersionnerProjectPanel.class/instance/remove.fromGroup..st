actions
remove: aDependency fromGroup: aGroup
	"Remove a dependency from a group or project"

	aDependency
		ifNotNil: [ 
			(UIManager default 
					confirm: 'Are your sure you want to remove ', aDependency asString
					label: 'Remove a group')
				ifTrue: [ 
					aGroup
						ifNil: [ project removeDependencyNamed: aDependency name ]
						ifNotNil: [ aGroup dependencies: (aGroup dependencies reject: [ :dependency | dependency = aDependency ]) ].
					groupList roots: project groups ] ]
