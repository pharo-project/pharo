accessing-menu
statusMenu: aMenu
	^ aMenu
		add: 'History' selector: #showHistoryMenu;
		add: 'Store result as progress reference' selector: #storeResultIntoTestCases;
		add: 'Show progress' selector: #showProgress; 
		yourself