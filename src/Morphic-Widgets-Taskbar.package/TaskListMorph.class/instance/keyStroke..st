event handling
keyStroke: event 
	"Process keys to switch task."
	
	event keyCharacter = Character escape ifTrue: [^self delete].
	event anyModifierKeyPressed ifFalse: [^self done].
	event keyString =  '<Opt-Shift-tab>' ifTrue: [^self selectPreviousTask].
	event keyString =  '<Opt-tab>'	ifTrue: [^self selectNextTask]