specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :c | 
			c 
				add: #menu height: self toolbarHeight;
				add: #text ];
		yourself