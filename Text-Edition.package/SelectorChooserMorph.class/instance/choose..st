action
choose: index
	"Set the given choice and ok."
	requestor ifNil: [^ self delete].
	requestor editor editingState selectorChooser ~= self 
		ifTrue: [^ self delete].
	requestor takeKeyboardFocus.
	super choose: index
	