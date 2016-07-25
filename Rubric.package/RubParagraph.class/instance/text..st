accessing
text: aText
	(aText string notEmpty and: [aText runs isEmpty]) 
		ifTrue: [ aText runs: (Array with: self defaultFontChange) ].
	text := aText