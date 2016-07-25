help topic creation
hierarchyWithMethodsFor: aClass
	 
	|root topic |
	root := HelpTopic named: 'System reference for ', aClass name.
	topic := (ClassAPIHelpBuilder new)
					rootToBuildFrom: aClass;
					addSubclasses: true;
					addMethods: true;
					subclassesAsSeparateTopic: true;
					build;
					topicToBuild.
	root addSubtopic: topic.
	^root				
	 
		 