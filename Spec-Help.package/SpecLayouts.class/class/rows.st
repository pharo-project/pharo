pages
rows
	^HelpTopic 
		title: 'Layouts in rows'
		contents: 'If you want to arrange your widgets in a row, that''s simple:
		
		
	^ SpecLayout composed
			newRow: [ :row |
				row
					add: #theList;
					add: #theButton
			];
			yourself'