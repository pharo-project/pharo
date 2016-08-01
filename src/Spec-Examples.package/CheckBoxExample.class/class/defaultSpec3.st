specs
defaultSpec3
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				newRow: [ :row | 
					row
						add: #button1 origin: 0 @ 0 corner: 0.3 @ 1;
						add: #button2 origin: 0.3 @ 0 corner: 0.6 @ 1;
						add: #button3 origin: 0.6 @ 0 corner: 1 @ 1 ]
					origin: 0 @ 0
					corner: 1 @ 0.3;
				add: #container origin: 0 @ 0.3 corner:1 @ 1 ];
		yourself