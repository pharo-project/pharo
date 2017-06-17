specs
buildDefaultSpec: col
	col
		newRow: #localDirectoryLocation
		height: self inputTextHeight;
		
		newRow: [:row | row 
			add: #subdirectoryLabel width: 100;
			add: #subdirectory ]
		height: self inputTextHeight;
			
		add: #createButton height: self buttonHeight 

