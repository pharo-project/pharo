actions
helpTopic
	^ helpTopicCache ifNil: [ helpTopicCache := rootTopic asHelpTopic ]