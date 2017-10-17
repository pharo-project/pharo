specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col newRow: [:row | row add: #x; add: #y ]];
		yourself
