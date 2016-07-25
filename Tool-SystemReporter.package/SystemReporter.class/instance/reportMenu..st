accessing-report
reportMenu: aMenu
	^ aMenu
		addTitle: 'Report' ;
		add: 'Copy to Clipboard' selector: #copyReportToClipboard;
		yourself