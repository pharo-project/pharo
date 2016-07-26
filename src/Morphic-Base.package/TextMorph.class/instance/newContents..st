accessing
newContents: stringOrText 
	"Accept new text contents."
	| newText embeddedMorphs |
	"If my text is all the same font, use the font for my new contents"
	newText := stringOrText isString ifTrue: [ | textSize |
		(text notNil
		  and: [ (textSize := text size) > 0
		    and: [ (text runLengthFor: 1) = textSize ]]) ifTrue: [ | attribs |
			attribs := text attributesAt: 1 forStyle: textStyle.
			Text string: stringOrText copy attributes: attribs.
		]
		ifFalse: [ Text fromString: stringOrText copy ]
	]
	ifFalse: [ stringOrText copy asText.	"should be veryDeepCopy?" ].

	(text = newText and: [text runs = newText runs]) ifTrue: [^ self].	"No substantive change"
	text ifNotNil: [(embeddedMorphs := text embeddedMorphs)
			ifNotNil: 
				[self removeAllMorphsIn: embeddedMorphs.
				embeddedMorphs do: [:m | m delete]]].

	text := newText.

	"add all morphs off the visible region; they'll be moved into the right 
	place when they become visible. (this can make the scrollable area too 
	large, though)"
	newText embeddedMorphs do: 
		[:m | 
		self addMorph: m.
		m position: -1000 @ 0].
	self releaseParagraph.
	"update the paragraph cache"
	self paragraph.
	"re-instantiate to set bounds"
	self world ifNotNil: [self world startSteppingSubmorphsOf: self]