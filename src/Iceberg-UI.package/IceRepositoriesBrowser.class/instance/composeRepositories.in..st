building
composeRepositories: repositories in: composite
	| table |

	table := composite fastTable.
	table
		title: 'Repositories';
		display: [ :repos | repos collect: [ :repo | IceRepositoryModel modelFor: repo ] ];
		column: 'Name' evaluated: #descriptionText width: 200;
		column: 'Current branch' evaluated: #branchName width: 120;
		column: 'Loaded version' evaluated: #loadedCodeDescription width: 200;
		column: 'Status' evaluated: #status width: 150.

	table updateOn: IceRepositoryRegistryModified from: [ Iceberg announcer weak ].
	repositories do: [ :repo | 
		table updateOn: IceRepositoryAnnouncement from: repo announcer ].
	table updateOn: MCPackageModified from: MCPackageManager announcer.
	
	Iceberg announcer weak 
		subscribe: IceRepositoryCreated 
		send: #execute:
		to: (IceRepositoryUpdateHelper for: table).

	table		
		"Task bar menu"
		addAction: self newRepositoryAction;
		addAction: self cloneRepositoryAction;
		addAction: self addLocalRepositoryAction;
		addAction: self fetchAllRepositoriesAction;
		"Repository pop up menu"
		addSelectionAction: self synchronizeRepositoryAction;
		addSelectionAction: self showHistoryAction;
		addSelectionAction: self switchBranchAction;
		addSelectionAction: self createNewBranchAction;
		addSelectionAction: self addRemoteAction;
		addSelectionAction: self pullAction;
		addSelectionAction: self pullFromAction;
		addSelectionAction: self pushAction;
		addSelectionAction: self pushToAction;
		addSelectionAction: self cloneRepositoryAgainAction;
		addSelectionAction: self refreshRepositoryAction;
		addSelectionAction: self editRepositoryAction;
		addSelectionAction: self forgetRepositoryAction;
		"Plugin actions"
		dynamicActionsOnSelection: [ :presentation | 
			presentation selection pluginActions ];
		"Double click action"
		onChangeOfPort: #strongSelection act: [ :presentation | 
			self synchronize: presentation selection repository ]