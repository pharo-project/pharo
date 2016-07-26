building
buildHelpTopicFrom: aHelpTopicDescription
	"Start building from the given help topic description"
	
	^(self new)
		rootToBuildFrom: aHelpTopicDescription;
		build;
		topicToBuild 