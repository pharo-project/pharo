spec
defaultSpec
	"Define the layout of the Versionner Browser"
	^ SpecLayout composed
		newColumn: [ : column | 
			column
				add: #toolbar height: self toolbarHeight;
				newRow: [ :mainPanel | mainPanel
					newColumn: [ :leftPanel | leftPanel
						newRow: [ :versionsHeader | 
							versionsHeader
								add: #versionsLabel;
								add: #allVersionsCheckBox ] 
						height: self toolbarHeight;					
						
						add: #versionList ] 
					right: 0.7;
					
					addSplitter;
					
					add: #projectPanel left: 0.3
				];
				newRow: [ :row |
					row
						newColumn: [  :col | 
							col
								newRow: [:repoHeader | repoHeader
									add: #repositoryLabel  width: 75;
									add: #repository;
									add: #repositoryAddButton width: self toolbarHeight ] 
								height: self toolbarHeight;
						
								newColumn: [:versionDescRow | versionDescRow 
									add: #versionDescriptionLabel height: self toolbarHeight;
									add: #versionDescription ];
								
								add: #versionCommitMessage height: self toolbarHeight ] ] height: 120
			
			];
	yourself.
 