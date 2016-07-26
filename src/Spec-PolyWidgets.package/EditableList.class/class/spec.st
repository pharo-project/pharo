spec
spec
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :column | 
			column
				newRow: [ :menuRow | 
							menuRow
								add: #addButton;
								add: #removeButton ]
					height: 25;
				newRow: [ :listRow | 
							listRow
								newColumn: [ :c1 | c1 add: #list ];
								newColumn: [ :c2 | 
											c2
												add: #topButton;
												add: #upButton;
												add: #downButton;
												add: #bottomButton ]
									width: 24 ] ] 
		yourself