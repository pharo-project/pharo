specs
spec
	<spec: #default>

	^ SpecLayout composed
		newColumn: [:c |
			c 
				newRow: [ :spec | spec
					add: #debugButton;
					add: #browseButton ] height: 30;
				newRow: [ :spec | spec add:  #errorListModel ];
				newRow: [ :spec | spec
						add: #timeStampLabel width: 100;
						add: #timeStamp ] height: 30;
				newRow: [ :spec | spec
						add: #imageVersionLabel width: 100;
						add: #imageVersion ] height: 30;
				newRow: [ :spec | spec
						add: #vmVersionLabel width: 100;
						add: #vmVersion ] height: 75];
		yourself.