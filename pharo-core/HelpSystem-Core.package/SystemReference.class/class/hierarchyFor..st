help topic creation
hierarchyFor: aClass
	 
	|root topic |
	root := HelpTopic named: 'System reference for ', aClass name.
	topic := (ClassAPIHelpBuilder new)
					rootToBuildFrom: aClass;
					addSubclasses: true;
					addMethods: false;
					subclassesAsSeparateTopic: false;
					build;
					topicToBuild.
	root addSubtopic: topic.
	^root				
	 
		 