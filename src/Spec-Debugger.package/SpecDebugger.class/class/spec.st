specs
spec
	<spec>
	
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #stack;
				add: #toolbar height: 25;
				addSplitter;
				add: #code;
				addSplitter;
				newRow: [ :row | 
					row
						add: #receiverInspector;
						add: #contextInspector ] ]