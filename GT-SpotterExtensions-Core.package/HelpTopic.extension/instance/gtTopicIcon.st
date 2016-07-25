*GT-SpotterExtensions-Core
gtTopicIcon
	^ self hasSubtopics
		ifTrue: [ Smalltalk ui icons iconNamed: #bookIcon ]
		ifFalse: [ Smalltalk ui icons iconNamed: #pageIcon ]