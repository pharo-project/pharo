accessing
extent: aPoint
	"Make the choices area at least fill the scroll area."

	|m|
	super extent: aPoint.
	m := self choicesMorph.
	m ifNotNil: [m width: (m width max: self scrollPane width)]