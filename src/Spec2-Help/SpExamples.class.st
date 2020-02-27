"
A SpecExamples is the first sub-book of SpecHelpTopics. It contains examples of using Spec.
See class side for selectors.

"
Class {
	#name : #SpExamples,
	#superclass : #SpHelpTopics,
	#category : #'Spec2-Help-Topics'
}

{ #category : #accessing }
SpExamples class >> bookName [
	^ 'Examples'
]

{ #category : #pages }
SpExamples class >> firstExample [
	^HelpTopic 
		title: 'A first example: ModelList'
		contents: 'Creating a specific UI always starts with the subclassing of SpPresenter. Each sub widget is stored into an instance variable of the newly created class. All the code presented here is already in the image so you can simply do: 
		
	ModelList browse

And follow the tutorial while browsing the code. So let''s subclass SpPresenter.


	SpPresenter subclass: #ModelList
	instanceVariableNames: ''list''
	classVariableNames: ''
	category: ''Spec-Examples''		
		
The first required step then is to instantiate and define the sub widgets. This step is done in the method initializeWidgets. It creates the list and populates it with the required classes, in alphabetical order.

	ModelList>>initializeWidgets
	list := self newList.
	list items: (AbstractWidgetPresenter allSubclasses
	sorted: [:a :b | a name < b name ]).
	self focusOrder add: list
	
The second required step is to define a layout, which is done on the class side. Since there is here only one sub widget, the layout is quite simple. It simply returns a layout that contains only the list:

	ModelList class>>#defaultSpec
	<spec: #default>
	^ SpecLayout composed
	add: #list;
	yourself
	
The three last methods to define on ModelList are a getter, a method to display the UI title and a method to register to list selection changes.

	"accessing"
	ModelList>>list
		^ list

	"api"
	ModelList>>title
		^ ''Widgets''

	"api-events"
	ModelList>>whenSelectedItemChanged: aBlock
		list whenSelectedItemChanged: aBlock

The first UI is now done. The result can be seen by executing the following snippet of code: 
	ModelList new openWithSpec.

'
]

{ #category : #pages }
SpExamples class >> moreExamples [
	^HelpTopic 
		title: 'More Examples'
		contents: 'If you browse the Spec-Examples package you will find a lot more examples that will help you understand Spec a little more.
		
ApplicationWithToolbar new openWithSpec

CheckBoxExample new openWithSpec

ClassMethodBrowser new openWithSpec

DropListExample new openWithSpec

DynamicWidgetChange new openWithSpec

ListSelectionPresenter new openWithSpec

MethodBrowser new openWithSpec

ModelList new openWithSpec

ProtocolBrowser new openWithSpec

ProtocolList new openWithSpec

ProtocolViewer new openWithSpec

RadioButtonGroupExample new openWithSpec

ScrollSyncExample new openWithSpec

TabsExample new openWithSpec

TextFieldExample new openWithSpec'
]

{ #category : #accessing }
SpExamples class >> pages [
	^#(firstExample secondExample moreExamples)
]

{ #category : #pages }
SpExamples class >> secondExample [
	^HelpTopic 
		title: 'A second example: Protocol List'
		contents: 'The next user interface is the protocol list. This UI combines two sub widgets: 
		a list
		a label.

So again, we start by subclassing SpPresenter

	SpPresenter subclass: #ProtocolList
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
]
