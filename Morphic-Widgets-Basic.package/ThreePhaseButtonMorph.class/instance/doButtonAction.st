button
doButtonAction
	"Perform the action of this button. Subclasses may override this method. The default behavior is to send the button's actionSelector to its target object with its arguments."

	(target notNil and: [actionSelector notNil]) 
		ifTrue: 
			[Cursor normal 
				showWhile: [target perform: actionSelector withArguments: arguments].
			target isMorph ifTrue: [target changed]]