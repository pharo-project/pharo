pages
initializeWidgets
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