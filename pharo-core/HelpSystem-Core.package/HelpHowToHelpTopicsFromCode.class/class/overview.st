pages
overview
	^HelpTopic
		title: 'Overview'
		contents:	
	'OVERVIEW
	
The help system allows you to provide own books and help texts. You can open the help browser on any object that is able to understand #asHelpTopic.

This method returns the root node of the displayed topic hierarchy:

      HelpBrowser openOn: myObject

Typically the object does not convert itself to a help topic structure, usually it dispatches to a builder (see HelpBuilder and subclasses) who does all this. 
	
A much more convenient and reproducable way is to implement custom help classes. This allows you to implement and manage your help texts using the standard development and code management tools. These custom help classes are subclasses of "CustomHelp" and are automatically included into the standard help browser. '