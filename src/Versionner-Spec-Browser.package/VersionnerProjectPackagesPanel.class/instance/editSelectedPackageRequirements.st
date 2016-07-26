actions
editSelectedPackageRequirements
	"Edit package requirements for the selected package"
	
	| selectedPackage editionWidget |

	selectedPackage := self packageBoundToSelection.
	selectedPackage ifNil: [ ^self ].
	editionWidget := (EditableList new: selectedPackage dependencies) 
		title: selectedPackage name , ' requirements';
		addItemBlock: [ self addPackageRequirement ];
		yourself.	
	editionWidget okAction: [ "propagate changes"
		selectedPackage dependencies: editionWidget list listItems.
		packagesList roots: project packages ].
	self window openModal: editionWidget openDialogWithSpec window
	