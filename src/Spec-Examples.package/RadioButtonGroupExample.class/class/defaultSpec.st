specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [ :row |
			row 
				add: #button1;
				add: #button2;
				add: #button3 ] height: self toolbarHeight;
		add: #container;
		yourself