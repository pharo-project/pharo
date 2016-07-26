menu declaring
toolsOn: aBuilder 
	<worldMenuExample>
	(aBuilder item: #Tools)
		order: 1.0;
		target: Smalltalk tools;
		icon: (Smalltalk ui icons iconNamed: #smallDoItIcon);
		withSeparatorAfter;
		with: [(aBuilder item: #Transcript) selector: #openTranscript; icon: Smalltalk tools transcript  taskbarIcon. 
			(aBuilder item: #'File Browser') selector: #openFileList; icon: Smalltalk tools fileList taskbarIcon.
			(aBuilder item: #'Process Browser') selector: #openProcessBrowser; icon: Smalltalk tools processBrowser taskbarIcon;
				 withSeparatorAfter]