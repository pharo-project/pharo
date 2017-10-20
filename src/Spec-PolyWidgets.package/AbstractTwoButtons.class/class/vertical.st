specs
vertical
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #firstButton;
				add: #secondButton ];
		yourself