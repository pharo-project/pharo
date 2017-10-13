specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #topButton;
				add: #upButton;
				add: #downButton;
				add: #bottomButton ];
		yourself