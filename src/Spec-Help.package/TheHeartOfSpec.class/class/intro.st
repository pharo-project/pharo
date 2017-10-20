pages
intro
	^HelpTopic 
		title: 'The Heart of Spec'
		contents: 'All user interfaces in Spec are constructed through the composition of existing user interfaces. To define a user interface, it is sufficient to define the model of the user interface. The UI elements that correspond to this model are instantiated by Spec, depending on the underlying UI framework. 
		
It is the composition of this model and these UI elements that makes up the resulting widget that is shown to the user, i.e. the resulting user interface. Hence, since all UIs are constructed through composition of other UIs, and it is sufficient to define the model to define the UI, the root class of all UIs is named ComposablePresenter. So, to define a new user interface, a subclass of ComposablePresenter needs to be created.

Spec is inspired by the MVP pattern. It is built around three axes that materialize themselves as the following three methods: 

ComposablePresenter>>#initializeWidgets
ComposablePresenter>>#initializePresenter
ComposablePresenter class>>#defaultSpec. 
	
These methods are hence typically found in the model for each user interface. In the following topics we describe the responsibility for each method, i.e. how these three work together to build the overall UI.'