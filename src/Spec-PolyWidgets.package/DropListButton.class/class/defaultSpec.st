specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #dropList origin: 0 @ 0 corner: 0.8 @ 1;
				add: #button origin: 0.8 @ 0 corner: 1 @ 1 ];
		yourself