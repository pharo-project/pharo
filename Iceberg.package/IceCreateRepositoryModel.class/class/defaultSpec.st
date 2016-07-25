specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed newColumn:
		[ :col | col
			newRow: [:row | row
				add: #remoteUrlLabel width: 100;
				add: #remoteUrl ]
			height: self inputTextHeight;
			
			newRow: [:row | row
				add: #localDirectoryLabel width: 100;
				add: #localDirectory;
				add: #changeLocationButton width: self buttonWidth  ]
			height: self inputTextHeight;
			
			newRow: [:row | row 
				add: #subdirectoryLabel width: 100;
				add: #subdirectory ]
			height: self inputTextHeight;
			
			add: #createButton height: self buttonHeight 
		]
