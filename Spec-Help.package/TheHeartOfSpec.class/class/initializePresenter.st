pages
initializePresenter
	^HelpTopic
		title: 'The initializePresenter method'
		contents: 'This method takes care of the interactions between the different widgets. By linking the behavior of the different widgets it specifies the overall presentation, i.e. how the overall UI responds to interactions by the user. Usually this method consists of specifications of actions to perform when a certain event is received by a widget. From the propagation of those events the whole interaction flow of the UI emerges. In Spec, the different UI models are contained in value holders, and the event mechanism relies on the announcements of these value holders to manage the interactions between widgets. 
Value holders provide a single method whenChangedDo: that is used to register a block to perform on change. In addition to this primitive whenChangedDo: method, the basic widgets provide more specific hooks, e.g. when an item in a list is selected or deselected.'