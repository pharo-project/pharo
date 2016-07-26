building
buildHierarchicalHelpTopicFrom: aClass withSubclasses: aBoolean withMethods: anotherBoolean
	"Start building from the given class"
	
	^(self new)
		addSubclasses: aBoolean;
		addMethods: anotherBoolean;
		rootToBuildFrom: aClass;
		build;
		topicToBuild 