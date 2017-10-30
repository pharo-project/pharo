pages
firstExample
	^HelpTopic 
		title: 'A first example: ModelList'
		contents: 'Creating a specific UI always starts with the subclassing of Composable-Model. Each sub widget is stored into an instance variable of the newly created class. All the code presented here is already in the image so you can simply do: 
		
	ModelList browse

And follow the tutorial while browsing the code. So let''s subclass ComposablePresenter.


	ComposablePresenter subclass: #ModelList
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