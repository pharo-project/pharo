menu
menuStackList: aBuilder
	<contextMenu>
	<processBrowserStackListMenu>
	
	(aBuilder item: #'Inspect context')
		keyText: 'c';
		selector: #inspectContext.
	(aBuilder item: #'Explore context')
		keyText: 'C';
		selector: #exploreContext;
		withSeparatorAfter.
		
	(aBuilder item: #'Inspect receiver')
		keyText: 'i';
		selector: #inspectReceiver.
	(aBuilder item: #'Explore receiver')
		keyText: 'I';
		selector: #exploreReceiver;
		withSeparatorAfter.
		
	(aBuilder item: #'Browse')
		keyText: 'b';
		selector: #browseContext.