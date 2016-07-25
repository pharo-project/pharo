specs
defaultSpec
	<spec>
	^ SpecLayout composed
		newColumn: [ :column1 | 
			column1
				newColumn: [ :column2 | column2 add: #changesTree ];
				addSplitter;
				add: #textArea ]