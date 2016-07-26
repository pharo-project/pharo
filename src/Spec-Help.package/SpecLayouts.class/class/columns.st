pages
columns
	^HelpTopic 
		title: 'Layouts in columns'
		contents: 'Having the widgets rendered as a column is easy:

	^ SpecLayout composed
			newColumn: [ :column |
				column
					add: #theList;
					add: #theButton
			];
			yourself'