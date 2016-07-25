A PluggableButtonMorph is a combination of an indicator for a boolean value stored in its model and an action button. The action of a button is often, but not always, to toggle the boolean value that it shows. Its pluggable selectors are:

		getStateSelector		fetch a boolean value from the model
		actionSelector		invoke this button's action on the model
		getLabelSelector		fetch this button's lable from the model
		getMenuSelector		fetch a pop-up menu for this button from the model

Any of the above selectors can be nil, meaning that the model does not supply behavior for the given action, and the default behavior should be used. For example, if getStateSelector is nil, then this button shows the state of a read-only boolean that is always false.

The model informs its view(s) of changes by sending #changed: to itself with getStateSelector as a parameter. The view tells the model when the button is pressed by sending actionSelector.

If the actionSelector takes one or more arguments, then the following are relevant:
		arguments			A list of arguments to provide when the actionSelector is called.
		argumentsProvider	The object that is sent the argumentSelector to obtain arguments, if dynamic
		argumentsSelector	The message sent to the argumentProvider to obtain the arguments.

Options:
	askBeforeChanging		have model ask user before allowing a change that could lose edits
	triggerOnMouseDown	do this button's action on mouse down (vs. up) transition
	shortcutCharacter		a place to record an optional shortcut key
