initialization
initializePresenter
	"Initialize widgets behavior"

	projectList whenSelectedNodesChanged: [ :selection | 
					removeProjectButton enabled: (self isProject: selection).
					editProjectButton enabled: selection notEmpty.
					projectList menu: [ :aMenu | self requiredProjectMenu: aMenu] ].
	addProjectButton action: [ self addDependentProject ].
	removeProjectButton
		action: [ projectList selectedItem 
					ifNotNil: [ :selectedItem | self removeDependentProject: selectedItem content ] ].
	editProjectButton action: [ self editSelectedProjectLoads ].
	
	groupList whenSelectedItemsChanged: [ :selection | removeGroupButton enabled: selection notEmpty ].
	addGroupButton action: [ self addGroup ].
	removeGroupButton
		action: [ 
			groupList selectedItem
				ifNotNil: [ :selectedItem | | parentGroup |
					parentGroup :=  groupList selectedItem parentNode
											ifNil: [ nil ]
											ifNotNil: [ :parentNode | parentNode content ].
					self 
						remove: selectedItem content 
						fromGroup: parentGroup ] ]