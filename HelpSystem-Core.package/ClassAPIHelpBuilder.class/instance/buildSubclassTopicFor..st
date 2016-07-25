private building
buildSubclassTopicFor: aSubclass
	 
	^(self class new) 
			rootToBuildFrom: aSubclass;
			addSubclasses: addSubclasses;
			addMethods: addMethods;
			subclassesAsSeparateTopic: subclassesAsSeparateTopic;
			build;
			topicToBuild 
	 