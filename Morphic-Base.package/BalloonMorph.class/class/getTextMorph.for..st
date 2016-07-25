private
getTextMorph: aStringOrMorph for: balloonOwner
	"Construct text morph."
	| m text |
	aStringOrMorph isMorph
		ifTrue: [m := aStringOrMorph]
		ifFalse: [balloonOwner balloonFont
				ifNil: [text := aStringOrMorph]
				ifNotNil: [text := Text
								string: aStringOrMorph
								attribute: (TextFontReference toFont: balloonOwner balloonFont)].
			m := (TextMorph new contents: text) centered].
	m setToAdhereToEdge: #adjustedCenter.
	^ m