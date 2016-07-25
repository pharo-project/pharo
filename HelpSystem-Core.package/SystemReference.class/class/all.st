help topic creation
all
	"HelpBrowser openOn: self all "
	
	^(ClassAPIHelpBuilder new)
		rootToBuildFrom: ProtoObject;
		addSubclasses: true;
		addMethods: true;
		subclassesAsSeparateTopic: false;
		build;
		topicToBuild 
	 
		 