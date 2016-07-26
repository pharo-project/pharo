button
doButtonAction
	"Perform the action of this button. The last argument of the message sent to the target is the new state of this switch."

	| newState |
	(target notNil and: [actionSelector notNil]) 
		ifTrue: 
			[newState := color = onColor.
			target perform: actionSelector
				withArguments: (arguments copyWith: newState)]