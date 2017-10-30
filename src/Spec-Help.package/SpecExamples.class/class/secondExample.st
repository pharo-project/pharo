pages
secondExample
	^HelpTopic 
		title: 'A second example: Protocol List'
		contents: 'The next user interface is the protocol list. This UI combines two sub widgets: 
		a list
		a label.

So again, we start by subclassing ComposablePresenter:

	ComposablePresenter subclass: #ProtocolList
	instanceVariableNames: ''label protocols''
	classVariableNames: ''
	category: ''Spec-Examples''
	
The initializeWidgets method for this UI is quite similar to the method in ModelList:

	ProtocolList>>initializeWidgets
		protocols := self newList.
		label := self newLabel.
		label text: ''Protocol''.
		protocols displayBlock: [ :m | m selector ].
		self focusOrder add: protocols
	
The layout method is quite different though. Now the sub widgets need to be placed more specifically than in the previous example, we have to build a layout that has a column with the label on top and the list taking all the space that is left.

	ProtocolList class>>defaultSpec
		<spec: #default>
		^ SpecLayout composed
		newColumn: [ :column |
			column
				add: #label
					height: self toolbarHeight;
					add: #protocols ];
		yourself
		
	
The remaining methods are getters, sub widget delegation methods, a method to display the title, and a method to register to list selection changes: 


	"accessing"
	ProtocolList>>label
		^ label

	"accessing"
	ProtocolList>>protocols
		^ protocols

	"api"
	ProtocolList>>items: aCollection
		protocols items: aCollection

	"api"
	ProtocolList>>label: aText
		label label: aText

	"api"
	ProtocolList>>resetSelection
		protocols resetSelection

	"api"
	ProtocolList>>title
		^ ''Protocol widget''

	"api-events"
	ProtocolList>>whenSelectedItemChanged: aBlock
		protocols whenSelectedItemChanged: aBlock

The ProtocolList UI can be seen by evaluating:

 ProtocolList new openWithSpec.
'