events-processing
keyStroke: event 
	"Process potential command keys."
	(self navigationKey: event) ifTrue: [^true].
	(self scrollByKeyboard: event) ifTrue: [^true].
	^ self listManager keyStroke: event