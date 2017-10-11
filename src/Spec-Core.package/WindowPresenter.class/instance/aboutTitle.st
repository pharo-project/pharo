api
aboutTitle
	"Answer the title of my About window"

	(self model respondsTo: #aboutTitle)
		ifTrue: [^self model aboutTitle].
	^(self model respondsTo: #toolName)
		ifTrue: [self model toolName]
		ifFalse: [self model class name].
