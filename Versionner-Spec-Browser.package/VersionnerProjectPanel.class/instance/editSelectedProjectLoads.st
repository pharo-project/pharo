actions
editSelectedProjectLoads
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
	