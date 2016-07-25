event handling
keyStroke: anEvent
	"Handle a keystroke event."
	anEvent keyCharacter = Character cr
		ifTrue: [ self handleReturnKey ifTrue: [ ^ self ] ].
	anEvent hand anyButtonPressed ifTrue: [ ^self ].
	self scrollPane ifNotNil: [ :sp | (sp scrollByKeyboard: anEvent) ifTrue: [^self] ].
	anEvent isUserInterrupt ifTrue: [^self].
	self handleEdit: [ self editor keystroke: anEvent ].
	self scrollSelectionIntoView: nil