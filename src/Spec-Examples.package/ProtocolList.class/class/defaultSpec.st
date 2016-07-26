specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed
		newColumn: [ :column |
			column 
				add: #label
				height: self toolbarHeight;
				add: #protocols ];
		yourself