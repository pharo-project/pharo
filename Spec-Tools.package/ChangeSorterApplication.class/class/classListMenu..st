menu
classListMenu: aBuilder
	<contextMenu>
	<changeSorterClassListMenu>
	
	(aBuilder item: #'Delete class from change set')
		keyText: 'd';
		selector: #forgetClass.
	(aBuilder item: #'Remove class from system')
		keyText: 'x';
		selector: #removeClass;
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b';
		selector: #browseMethodFull;
		withSeparatorAfter.
		
	(aBuilder item: #'FileOut')
		selector: #fileOutClass.
