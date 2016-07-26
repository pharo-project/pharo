actions
selectTopicSatisfying: aBlock
	"I select the first topic that satisfies the given block.
	If the topic has subtopics it is expanded. If the topic is not visible 
	the entire path to that topic is expanded."
	
	(self helpTopic pathToTopicSatisfying: aBlock) ifNotEmpty: [ :aCollection |
		treeMorph 
			selectPath: aCollection allButFirst
			in: (treeMorph scroller submorphs at: 1 ifAbsent: [^treeMorph]) ]