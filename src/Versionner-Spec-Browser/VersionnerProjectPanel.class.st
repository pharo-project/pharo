"
A VersionPanel is the panel used to show details about a given version of a configurationOf.

Instance Variables
	config: 				The configuration (an MBConfigurationInfo instance) of the project to display details on. 
	packageList:			The specWidget used to display packages describes in the version

"
Class {
	#name : #VersionnerProjectPanel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'config',
		'packagesPanel',
		'projectList',
		'groupList',
		'groupsLabel',
		'projectsLabel',
		'project',
		'addGroupButton',
		'removeGroupButton',
		'addProjectButton',
		'removeProjectButton',
		'editProjectButton'
	],
	#category : #'Versionner-Spec-Browser'
}

{ #category : #spec }
VersionnerProjectPanel class >> defaultSpec [
	"Define the layout of the Project Panel"

	<spec>
	^ SpecLayout composed
		newColumn: [ :c1 | 
					c1
						newColumn: [ :c11 | 
							c11
								newRow: [ :r1 | 
									r1
										add: #projectsLabel origin: 0@0 corner: 1@1;
										add: #addProjectButton width: 50;
										add: #removeProjectButton width: 50;
										add: #editProjectButton width: 50]
								height: 25;
								add: #projectList ];
						addSplitter;
						newColumn: [ :c12 | 
							c12
								newRow: [ :r2 | 
									r2
										add: #groupsLabel origin: 0@0 corner: 1@1;
										add: #addGroupButton width: 50;
										add: #removeGroupButton width: 50]
								height: 25;
							add: #groupList ];
						addSplitter;
						add: #packagesPanel ];
		yourself
]

{ #category : #actions }
VersionnerProjectPanel >> addDependentProject [
	"Add a new dependent project to the configuration"

	| requiredProjectNames dependentProject searchUI |
	project
		ifNotNil: [
			requiredProjectNames := project requiredProjects collect: [ :reqProject | reqProject name ].
			searchUI :=  SearchFacade projectWithConfigurationSearchRejectAll: [ :item | 
				(requiredProjectNames includes: item name configurationBaseName) 
					or: [ project name = item name configurationBaseName ] ].
			dependentProject := searchUI chooseFromOwner: self window.
			dependentProject ifNotNil: [ project addDependency: (MTDependantProject newWithWorkingCopy: dependentProject) ].
			projectList roots: project requiredProjects ]
]

{ #category : #actions }
VersionnerProjectPanel >> addGroup [
	"Add a new group to the configuration"

	| groupName |
	project
		ifNotNil: [ 
			groupName := UIManager default request: 'New group:'.
			groupName ifNotNil: [ project addDependency: (MTGroup new name: groupName) ].
			groupList roots: project groups ]
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> addGroupButton [
	^ addGroupButton
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> addProjectButton [
	^ addProjectButton
]

{ #category : #actions }
VersionnerProjectPanel >> addProjectLoadDirective [
	"Add a load directive to the selected project."
	
	projectList selectedItem
		ifNil: [ UIManager default inform: 'No project selected' ]
		ifNotNil: [ :requiredProject |
			^ UIManager default request: 'Package or group to load?' 
			"|  potentialRequirements |
			potentialRequirements := project dependencies collect: [ :dependency | dependency name ].
			^ UIManager default chooseFrom: potentialRequirements values: potentialRequirements" ].
	^ nil
]

{ #category : #actions }
VersionnerProjectPanel >> browseProject: aConfigurationClass [
	| configInfo |
	configInfo := MBConfigurationRoot current configurationInfoFor: aConfigurationClass.
	configInfo ifNotNil: 
		[ configInfo recalculate.
		^ VersionnerProjectBrowser openOn: configInfo ]
]

{ #category : #actions }
VersionnerProjectPanel >> browseSelectedProject [
	^ self projectBoundToSelection
		ifNotNil: [ :dependantProject | 
			self browseProject: dependantProject configurationClass ]
]

{ #category : #api }
VersionnerProjectPanel >> config: anMBConfigurationInfo [	

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
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> editProjectButton [
	^ editProjectButton
]

{ #category : #actions }
VersionnerProjectPanel >> editSelectedProjectLoads [
	"Edit load directives for the selected project"
	
	| selectedProject editionWidget |

	selectedProject := self projectBoundToSelection.
	selectedProject ifNil: [ ^self ].
	editionWidget := (EditableList new: selectedProject dependencies) 
		title: selectedProject name , ' load directives';
		addItemBlock: [ self addProjectLoadDirective ];
		yourself.	
	editionWidget okAction: [ "propagate changes"
		selectedProject dependencies: editionWidget list listItems.
		projectList roots: project requiredProjects ].
	self window openModal: editionWidget openDialogWithSpec window
	
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> groupList [
	^ groupList
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> groupsLabel [
	^ groupsLabel
]

{ #category : #initialization }
VersionnerProjectPanel >> initializeGroupsWidget [
	groupList := (self instantiate: TreePresenter)
		autoDeselection: true;
		dragEnabled: true;
		dropEnabled: true;
		acceptDropBlock: [ :transfer :event :source :receiver | | passengerItem myreceiver | 
			myreceiver := receiver.
			myreceiver parentNode ifNotNil: [ myreceiver := myreceiver parentNode ]. "Only add dependencies on top elements"
			passengerItem := transfer passenger item.
			myreceiver content  
				addDependency: ((passengerItem isKindOf: MTDependency) "Ugly test because String>>name adds extra quotes"
					ifTrue: [ passengerItem name ]
					ifFalse: [ passengerItem ]) ];
		whenBuiltDo: [ groupList rootNodeHolder: 
							[ :item | VSGroupNode new content: item ] ];
		yourself.
	
	addGroupButton := (self instantiate: ButtonPresenter)
		label: nil;
		icon: (self iconNamed: #addIcon);
		enabled: false;
		help: 'Add a new group to this project'.
	
	removeGroupButton  := (self instantiate: ButtonPresenter)
		label: nil;
		icon: (self iconNamed: #deleteIcon);
		enabled: false;
		help: 'Remove a group from this project'.
]

{ #category : #initialization }
VersionnerProjectPanel >> initializePresenter [
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
]

{ #category : #initialization }
VersionnerProjectPanel >> initializeProjectsWidget [
	projectList := (self instantiate: TreePresenter)
		whenBuiltDo: [ projectList rootNodeHolder: [ :item | VSProjectNode new content: item ] ];
		on: PharoShortcuts current browseShortcut
			do: [ self browseSelectedProject ];
		doubleClick: [ self browseSelectedProject ];
		yourself.	"projectList displayBlock: [:item | item asString ].
	projectList childrenBlock: [:item | 
		(item isKindOf: MTDependency )
			ifTrue: [ item dependencies collect: [ :each | '--> loads: ' , each] ] 
			ifFalse: [{}] ]."	"| text version |
		text :=  item name.
		version := item currentVersion.
		(version size > 0) ifTrue: [
			text := text , ' (' , item currentVersion , ')' ].
		text  asStringMorph ]."
	addProjectButton := (self instantiate: ButtonPresenter)
		label: nil;
		icon: (self iconNamed: #addIcon);
		enabled: false;
		help: 'Add a new dependent project to this project'.
	removeProjectButton := (self instantiate: ButtonPresenter)
		label: nil;
		icon: (self iconNamed: #deleteIcon);
		enabled: false;
		help: 'Remove a dependent project from this project'.
	editProjectButton := self newButton
		label: nil;
		icon: (self iconNamed: #editIcon);
		enabled: false;
		help: 'Edit the dependent project loads list'
]

{ #category : #initialization }
VersionnerProjectPanel >> initializeWidgets [
	self initializeGroupsWidget.
	self initializeProjectsWidget.
	
	packagesPanel := self instantiate: VersionnerProjectPackagesPanel.

	groupsLabel  := (self instantiate: LabelPresenter)
		label: 'Groups';
		yourself.
	
	projectsLabel :=  self instantiate: LabelPresenter.
	projectsLabel label: 'Dependent projects'.

]

{ #category : #testing }
VersionnerProjectPanel >> isProject: selection [
	| node |
	
	(selection size = 1) ifFalse: [  ^false ].
	node := selection at: 1.
	^ node parentNode isNil
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> packagesPanel [
	^ packagesPanel
]

{ #category : #accessing }
VersionnerProjectPanel >> project [
	^ project
]

{ #category : #api }
VersionnerProjectPanel >> project: anMTProject [
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
]

{ #category : #selection }
VersionnerProjectPanel >> projectBoundToSelection [
	| selection |
	
	selection := projectList selectedItem.
	selection ifNil: [ ^ nil ].
	^ (selection parentNode)
		ifNil: [ selection content ]
		ifNotNil: [ selection parentNode content ]
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> projectList [
	^ projectList
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> projectsLabel [
	^ projectsLabel
]

{ #category : #refreshing }
VersionnerProjectPanel >> refreshGroups [
	groupList roots: project groups
]

{ #category : #actions }
VersionnerProjectPanel >> remove: aDependency fromGroup: aGroup [
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

]

{ #category : #actions }
VersionnerProjectPanel >> removeDependentProject: aMTDependantProject [ 
	aMTDependantProject remove.
	projectList roots: project requiredProjects.
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> removeGroupButton [
	^ removeGroupButton
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> removeProjectButton [
	^ removeProjectButton
]

{ #category : #'accessing-ui' }
VersionnerProjectPanel >> requiredProjectMenu: aMenu [
	aMenu addMenuGroup: (MenuGroupPresenter new
		addMenuItem: (MenuItemPresenter new
			name: #'browse project';
			action: [ self browseSelectedProject ]);
		addMenuItem: (MenuItemPresenter new
			name: #'edit version';
			action: [ self projectBoundToSelection editVersion.
					projectList roots: project requiredProjects ]);
		addMenuItem: (MenuItemPresenter new
			name: #'edit repositories';
			action: [ self projectBoundToSelection editRepositories ]);
		yourself).

	^ aMenu
]
