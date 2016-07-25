pages
overview
	^HelpTopic 
		title: 'Overview'
		contents:	
	'THE IMPLEMENTATION

The help system typically consists of help books including one or more pages. A book or page is therefore a "topic of interest"  providing contents for help to a user. A topic has a title and an icon and is able to have subtopics forming a hierarchy of topics. This simple model is reflected in the class HelpTopic.

Since this model forms a hierarchical structure of help topics there is a browser with a tree to display the help contents. This browser is implemented in class HelpBrowser.

You can open this browser programmatically using:

   HelpBrowser open
' 