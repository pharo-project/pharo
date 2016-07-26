accessing
addSubtopic: aTopic
	"Adds the given topic to the receivers collection of subtopics, and set this object as owner of the subtopic"
	
	self subtopics add: aTopic.
	aTopic owner: self.
	^aTopic