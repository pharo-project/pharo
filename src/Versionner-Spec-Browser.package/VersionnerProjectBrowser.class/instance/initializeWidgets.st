initialization
initializeWidgets
	"toolbar"
	toolbar := self instantiate: VersionnerProjectToolBar.
	toolbar browser: self.
	
	"repository"
	(repositoryLabel := self instantiate: LabelPresenter)
		label: 'Repository:'.
	(repository := self instantiate: TextInputFieldPresenter)
		entryCompletion: nil;
		acceptOnCR: true;
		ghostText: 'none'.
	repositoryAddButton := (self instantiate: ButtonPresenter)
		icon: (self iconNamed: #monticelloPackageIcon);
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
	versionsLabel := (self instantiate: LabelPresenter)
		label: 'Versions';
		yourself.
	allVersionsCheckBox := (self instantiate: CheckBoxPresenter)
		label: 'browse all';
		state: false;
		yourself.

	"Version details"
	versionDescriptionLabel := (self instantiate: LabelPresenter)
		label: 'Version description:';
		yourself.
	versionDescription := (self instantiate: TextPresenter)
		enabled: false;
		yourself.
	versionCommitMessage := self instantiate: LabelPresenter.
	
	"Project details"
	projectPanel := self instantiate: VersionnerProjectPanel.
	