pages
widgetInstantiation
	^HelpTopic
		title: 'Widget Instantiation'
		contents: 'The instantiation of the model for a widget (and hence the widget) can be done in two ways: 
Through the use of an creation method or through the use of the instantiate: method. Considering the first option, the framework provides unary messages for the creation of all basic widgets. The format of these messages is:   
	new[Widget]

for example newButton creates a button widget, and newList creates a list widget. 
The complete list of available widget creation methods can be found in the class Composable-Model in the protocol widgets. 

Considering the second option, to reuse any composite widgets, i.e. a subclass of ComposablePresenter, the widget needs to be initialized using the instantiate: method. For example, to reuse a Message-Browser widget, the code is:

	self instantiate: MessageBrowser.'