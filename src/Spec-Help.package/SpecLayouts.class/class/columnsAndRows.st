pages
columnsAndRows
	^HelpTopic 
		title: 'Combining Columns and Rows'
		contents: 'Rows and columns can be combined to build more complex layouts, and splitters between cells can be added. The example 1.24 shows how to create a 3 columns layout, containing three buttons in each column. This example also shows the addSplitter message, which adds a splitter between the element added before it and the element added after.


	^ SpecLayout composed
			newRow: [ :row |
				row
					newColumn: [ :column |
						column
							add: #button1;
							add: #button2;
							add: #button3
					];
					addSplitter;
					newColumn: [ :column |
						column
							add: #button4;
							add: #button5;
							add: #button6
					];
					addSplitter;
					newColumn: [ :column |
						column
							add: #button7;
							add: #button8;
							add: #button9
					];
			];
			yourself

The height of rows as well as the width of columns can be specified, to prevent them to take all the available space.  Here is an example of a row with a height of 30.

		^ SpecLayout composed
				newRow: [ :row |
					row
						add: #theList;
						add: #theButton
				] height: 30;
				yourself
				
				
And the same but for a column:

		^ SpecLayout composed
				newColumn: [ :column |
					column
						add: #theList;
						add: #theButton
				] width: 30;
			    yourself
			
			
Note that it is generally considered a bad habit to hardcode the size of the widgets. Methods are available on ComposablePresenter providing sensible default sizes, like the width of a button. When specifying custom widget sizes, care should be taken to take in account the current font size.'
			
		
