spec
spec
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :column | 
			column
				newRow: [ :menuRow | 
					menuRow
						add: #addButton;
						add: #removeButton ]
					height: 25;
				newRow: [ :listRow | 
					listRow
						add: #list ;
						add: #topUpDownBottomButtons width: 24 ] ] yourself