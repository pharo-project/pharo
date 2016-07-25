specs
defaultSpec
<spec>
	^ SpecLayout composed
		newRow: [:row | row
						add: #listModel;
						add: #methodModel ];
		yourself