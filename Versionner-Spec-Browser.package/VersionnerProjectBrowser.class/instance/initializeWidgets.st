initialization
initializeWidgets
	"toolbar"
	toolbar := self instantiate: VersionnerProjectToolBar.
	toolbar browser: self.
	
	"repository"
	(repositoryLabel := self instantiate: LabelModel)
		label: 'Repository:'.
	(repository := self instantiate: TextInputFieldModel)
		entryCompletion: nil;
		acceptOnCR: true;
		ghostText: 'none'.
	repositoryAddButton := (self instantiate: ButtonModel)
		icon: (Smalltalk ui icons iconNamed: #monticelloPackageIcon);
		help: 'Add a new repository to this project';
		yourself.
	
	"versions"
	versionList := (self instantiate: VersionListModel )
		icons: [:version | self iconForVersion: version ];
		on: PharoShortcuts current browseShortcut do: [ versionList selectedItem 
			ifNotNil: [ :selection |
				(VSBrowseVersionCommand target: selection version for: self) execute ] ];
		changedBlock: [ self hasChanges ];
		yourself.
	versionsLabel := (self instantiate: LabelModel)
		label: 'Versions';
		yourself.
	allVersionsCheckBox := (self instantiate: CheckBoxModel)
		label: 'browse all';
		state: false;
		yourself.

	"Version details"
	versionDescriptionLabel := (self instantiate: LabelModel)
		label: 'Version description:';
		yourself.
	versionDescription := (self instantiate: TextModel)
		enabled: false;
		yourself.
	versionCommitMessage := self instantiate: LabelModel.
	
	"Project details"
	projectPanel := self instantiate: VersionnerProjectPanel.
	