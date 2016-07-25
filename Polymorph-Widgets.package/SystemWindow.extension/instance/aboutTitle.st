*Polymorph-Widgets
aboutTitle
	"Answer the title to use for the About dialog."

	|title|
	title := self model
		ifNil: ['SystemWindow']
		ifNotNil: [(self model respondsTo: #aboutTitle)
					ifTrue: [self model aboutTitle]
					ifFalse: [self model class name]].
	^'About {1}' translated format: {title}