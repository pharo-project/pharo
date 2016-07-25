specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :col |
			col
				add: #menu height: (self toolbarHeight);
				add: #list;
				add: #inspectIt height: self toolbarHeight ]