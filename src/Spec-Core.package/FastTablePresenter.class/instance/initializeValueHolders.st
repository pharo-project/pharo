initialization
initializeValueHolders

	super initializeValueHolders.

	iconHolder := [:item | nil ] asValueHolder.
	handlesDoubleClick := false asValueHolder.
	doubleClick := [  ] asValueHolder.
