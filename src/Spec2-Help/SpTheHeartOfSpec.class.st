"
A SpecExamples is the second sub-book of SpecHelpTopics. It contains the core of Spec briefly explained.
See class side for selectors.

"
Class {
	#name : #SpTheHeartOfSpec,
	#superclass : #SpHelpTopics,
	#category : #'Spec2-Help-Topics'
}

{ #category : #accessing }
SpTheHeartOfSpec class >> bookName [
	^'The Heart of Spec'
]

{ #category : #pages }
SpTheHeartOfSpec class >> connectPresenters [
	^HelpTopic
		title: 'The initializePresenter method'
		contents: 'This method takes care of the interactions between the different widgets. By linking the behavior of the different widgets it specifies the overall presentation, i.e. how the overall UI responds to interactions by the user. Usually this method consists of specifications of actions to perform when a certain event is received by a widget. From the propagation of those events the whole interaction flow of the UI emerges. In Spec, the different UI models are contained in value holders, and the event mechanism relies on the announcements of these value holders to manage the interactions between widgets. 
Value holders provide a single method whenChangedDo: that is used to register a block to perform on change. In addition to this primitive whenChangedDo: method, the basic widgets provide more specific hooks, e.g. when an item in a list is selected or deselected.'
]

{ #category : #pages }
SpTheHeartOfSpec class >> initializePresenters [
	^HelpTopic
		title: 'The initializeWidgets method'
		contents: 'This method is used to instantiate the models for the different widgets that will be part of the UI and store them in their respective instance variables. Instantiation of the models will in turn result in the instantiation and initialization of the different widgets that make up the UI. Consequently, configuration and default values of each widget are specified here as well, which is why this method is called initializeWidgets. This focus in this method is to specify what the widgets will look like and what their self-contained behavior is. 
The behavior to update model state, e.g. when pressing a Save button, is described in this method as well. It is explicitly not the responsibility of this method to define the interactions between the widgets.

In general the initializeWidgets method should follow the pattern:
• widgets instantiation
• widgets configuration specification
• specification of order of focus

The last step is not mandatory but highly recommended. Indeed, without this final step keyboard navigation will not work at all.

An example of the initializeWidgets method could be:

	initializeWidgets
		theButton := self newButton.
		theList := self newList.
		theButton label: ''I am a button''.
		self focusOrder
		add: theButton;
		add: theList.

Specifying this method is mandatory, as without it the UI would have no widgets.'
]

{ #category : #pages }
SpTheHeartOfSpec class >> intro [
	^HelpTopic 
		title: 'The Heart of Spec'
		contents: 'All user interfaces in Spec are constructed through the composition of existing user interfaces. To define a user interface, it is sufficient to define the model of the user interface. The UI elements that correspond to this model are instantiated by Spec, depending on the underlying UI framework. 
		
It is the composition of this model and these UI elements that makes up the resulting widget that is shown to the user, i.e. the resulting user interface. Hence, since all UIs are constructed through composition of other UIs, and it is sufficient to define the model to define the UI, the root class of all UIs is named SpPresenter. So, to define a new user interface, a subclass of SpPresenter needs to be created.

Spec is inspired by the MVP pattern. It is built around three axes that materialize themselves as the following three methods: 

SpPresenter>>#initializeWidgets
SpPresenter>>#initializePresenter
SpPresenter class>>#defaultSpec. 
	
These methods are hence typically found in the model for each user interface. In the following topics we describe the responsibility for each method, i.e. how these three work together to build the overall UI.'
]

{ #category : #pages }
SpTheHeartOfSpec class >> layouts [
	^HelpTopic
		title: 'The layout method'
		contents: 'This method specifies the layout of the different widgets in the UI. It also specifies how a widget reacts when the window is resized. For the same UI multiple layouts can be described, and when the UI is built a specific layout to use can be specified. If no such specific layout is given, the following lookup mechanism will be used to obtain the layout method:

1. Search on class side, throughout the whole class hierarchy, for a method with the pragma <spec: #default>.
2. If multiple such methods exist, the first one found is used.
3. If none such methods exist and if there is exactly one method with the pragma <spec>, this method is used.
4. No layout method is found, an error is raised.

This method is on class side because it returns a value that usually is the same for all the instances. Put differently, usually all the instances of the same user interface have the same layout and hence this can be considered as being a class-side accessor for a class variable. Note that the lookup for the spec method to use starts on instance side, which allows a UI to have a more specific layout depending on the state of the instance.

In its simplest form, this method could be:

	^ SpecLayout composed
			add: #theList;
			yourself


The symbol theList refers to an instance side method returning a widget. This is because instance variables are private, so the layout class needs to use an accessor to obtain it when building the UI. Note that by default, a widget will take all the space available in its container.
This method is not restricted to laying out sub widgets. It can also refer to sub widgets contained in sub widgets, i.e. when reusing an existing UI, specify a new layout for the sub widgets that comprise this UI. To do this, instead of giving a symbol, an array with 2 symbols must be given. 
The first symbol identifies the UI being reused and the second the sub widget within this UI whose new layout position is being specified. 
As said above, multiple layouts can be described for the same user interface. In order to retrieve the correct method to apply, these methods need to be flagged with a pragma. The pragma can be either <spec: default> for the layout to use by default, or <spec> for the other layouts.

Specifying this method is mandatory, as without it the UI would show no widgets to the user.'

]

{ #category : #accessing }
SpTheHeartOfSpec class >> pages [
	^#(intro #initializePresenters widgetInstantiation #connectPresenters layouts)
]

{ #category : #pages }
SpTheHeartOfSpec class >> widgetInstantiation [
	^HelpTopic
		title: 'Widget Instantiation'
		contents: 'The instantiation of the model for a widget (and hence the widget) can be done in two ways: 
Through the use of an creation method or through the use of the instantiate: method. Considering the first option, the framework provides unary messages for the creation of all basic widgets. The format of these messages is:   
	new[Widget]

for example newButton creates a button widget, and newList creates a list widget. 
The complete list of available widget creation methods can be found in the class Composable-Model in the protocol widgets. 

Considering the second option, to reuse any composite widgets, i.e. a subclass of SpPresenter, the widget needs to be initialized using the instantiate: method. For example, to reuse a Message-Browser widget, the code is:

	self instantiate: MessageBrowser.'
]
