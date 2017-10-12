initialization
initializeWidgets
	packagesList := (self instantiate: TreePresenter)
		dragEnabled: true;
		whenBuiltDo: [ packagesList rootNodeHolder: [ :item | VSPackageNode new content: item ] ];
		yourself.  
	"packagesList icons: [:configuration | self iconForConfiguration: configuration ]. "
	"packagesList displayBlock: [:item |  |attributes text version|
							attributes := OrderedCollection new.
							text := item packageName.
							(item name size) > (text size) ifTrue: [
								version := (item name copyFrom: (text size + 2) to: (item name size)).
								(version size > 0) ifTrue: [ 
									text := text , ' (' , version , ')'. ].
								].
							item isDirty
								ifTrue: [ text := '*', text ].
							text.
						 ]."

	packagesLabel := self instantiate: LabelPresenter .
	packagesLabel label: 'Packages' .
	
	addPackageButton := self instantiate: ButtonPresenter .
	addPackageButton
		icon: (self iconNamed: #packageAddIcon);
		enabled: false;
		help: 'Add a package to this project'.
	
	removePackageButton  := self instantiate: ButtonPresenter .
	removePackageButton 
		icon: (self iconNamed: #packageDeleteIcon);
		enabled: false;
		help: 'Remove a package from this project'.
	
	editPackageButton  := self instantiate: ButtonPresenter .
	editPackageButton
		icon: (self iconNamed: #editIcon);
		enabled: false;
		help: 'Edit package requirements'.
	
	savePackageButton  := self instantiate: ButtonPresenter .
	savePackageButton
		icon: (self iconNamed: #smallSaveIcon);
		enabled: false;
		help: 'Save the selected package'.