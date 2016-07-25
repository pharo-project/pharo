specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed
		newRow: [ :topHalf |
			topHalf
				add: #tree;
				addVSplitter;
				newColumn: [ :c ] width: 4;
				newColumn: [ :commitInfoColumn |
					commitInfoColumn
						newRow: [ :commitRow | 
							commitRow 
								add: #commitMessage;
								add: #previousMessages width: self buttonHeight
						] height: self buttonHeight;
						add: #message;
						newRow: [ :authorRow |
							authorRow 
								add: #authorName width: 50;
								add: #author
						] height: self inputTextHeight +4 ;
						newRow: [ :repoRow |
							repoRow 
								add: #remoteLabel width: 50;
								add: #remotes;
								add: #manageRemotes width: self buttonHeight
						] height: self buttonHeight
				]
		] bottom: 0.5;
		addHSplitter;
		newRow: [ :row | row add: #diff ] top: 0.5;
		yourself