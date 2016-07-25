private
internalResetSelection

	selectionHolder reset.
	self getList ifNotEmpty: [ self silentlySetSelectedIndex: 1 ]