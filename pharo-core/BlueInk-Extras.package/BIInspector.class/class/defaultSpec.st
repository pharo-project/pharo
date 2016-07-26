specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn:
				[ :col | 
			col
				newRow: [ :rowUp | rowUp add: #settingsTree ];
				newRow:
						[ :rowMiddle | 
					rowMiddle
						add: #classSearchField;
						add: #methodSearchField;
						add: #formattedCheckBox ]
					height: 30;
				newRow:
						[ :rowDown | rowDown newColumn: [ :methodSourceCode | methodSourceCode add: #sourceCodePane ] ] ];
		yourself