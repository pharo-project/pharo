instance creation
openOn: aHelpTopic
	"Open the receiver on the given help topic or any other object that can be transformed into
	 a help topic by sending #asHelpTopic."
	
	^(self defaultHelpBrowser new)
		rootTopic: aHelpTopic;
		open;
		yourself