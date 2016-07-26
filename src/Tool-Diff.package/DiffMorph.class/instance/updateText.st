user interface
updateText
	"Reset the text if we have some."

	(self sourceTextModel getString notNil and: [ self destTextModel getString notNil ])
		ifTrue: [ self from: self sourceTextModel getString to: self destTextModel getString ]