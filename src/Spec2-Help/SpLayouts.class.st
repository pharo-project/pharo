"
A SpecExamples is the third sub-book of SpecHelpTopics. Here you will find more complex ways of laying out Spec.
See class side for selectors.

"
Class {
	#name : #SpLayouts,
	#superclass : #SpHelpTopics,
	#category : #'Spec2-Help-Topics'
}

{ #category : #accessing }
SpLayouts class >> bookName [
	^'Spec Layouts'
]

{ #category : #pages }
SpLayouts class >> columns [
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
]

{ #category : #pages }
SpLayouts class >> columnsAndRows [
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
			
			
Note that it is generally considered a bad habit to hardcode the size of the widgets. Methods are available on SpPresenter providing sensible default sizes, like the width of a button. When specifying custom widget sizes, care should be taken to take in account the current font size.'
			
		

]

{ #category : #pages }
SpLayouts class >> intro [
	^HelpTopic 
		title: 'Layouts'
		contents: 'As layouts can become quite complex, this section provides a list of examples of the construction of layouts.  All the methods for adding sub widgets can be found in the ''commands'' and ''commands-advanced'' protocols of the SpecLayout class. You can explore them by doing:
			SpecLayout browse
		
		
Often the layout of user interfaces can be described in rows and columns, and Spec provides for an easy way to specify such layouts.'
]

{ #category : #accessing }
SpLayouts class >> pages [
	^#(intro columns rows columnsAndRows)
]

{ #category : #pages }
SpLayouts class >> rows [
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
]
