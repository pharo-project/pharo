specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #input;
				add: #plusMinusButtons ];
			yourself 