event handling
doButtonAction: evt
	
	"Perform the action of this button. Subclasses may override this method. The default behavior is to send the button's actionSelector to its target object with its arguments."

	target ifNil: [^self].
	actionSelector ifNil: [^self].
	Cursor normal showWhile: [ | moreArgs |
		moreArgs := actionSelector numArgs > arguments size ifTrue: [
			arguments copyWith: evt
		] ifFalse: [
			arguments
		].
		target perform: actionSelector withArguments: moreArgs
	]