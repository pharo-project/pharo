accessing
icon
	"Either return the icon for the given topic"

	item icon notNil
		ifTrue: [ ^ item icon ].
	^ item hasSubtopics
		ifTrue: [ self iconNamed: #bookIcon ]
		ifFalse: [ self iconNamed: #pageIcon ]