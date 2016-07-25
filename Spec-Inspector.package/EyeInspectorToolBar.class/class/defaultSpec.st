specs
defaultSpec
	<spec>
	^ SpecLayout composed
		newRow: [ :r | 
			r
				add: #previousButton width: 24;
				add: #nextButton width: 28;
				add: #inspectorChoice
		];
	yourself.