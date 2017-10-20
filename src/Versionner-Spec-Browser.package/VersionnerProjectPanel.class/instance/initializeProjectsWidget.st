initialization
initializeProjectsWidget
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