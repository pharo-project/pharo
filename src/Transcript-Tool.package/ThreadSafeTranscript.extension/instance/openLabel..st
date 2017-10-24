*Transcript-Tool
openLabel: aString
	| window m |
	window := (SystemWindow labelled: aString) model: self.
	m := RubPluggableTextMorph new
		getTextSelector: #contents;
		setTextSelector: #clear;
		on: self;
		beForSmalltalkScripting.
	window addMorph: m frame: (0 @ 0 corner: 1 @ 1).
	^ window openInWorld
