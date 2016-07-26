menu
menuItems
	"Answer the menu items available for this tool set"

	^#(
		('System Browser' 			#openClassBrowser)
		-
		('Workspace'				#openWorkspace)
		('Transcript' 				#openTranscript)
		('File Browser'				#openFileList)
		-
		('Test Runner'				#openTestRunner)
		('Process Browser' 			#openProcessBrowser)
		-
		('Monticello Browser'		#openMonticelloBrowser)
		('Change Sorter'			#openChangeSorter)
	)
