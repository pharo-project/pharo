private-focus
ensureKeyBindingsFor: widget

	super ensureKeyBindingsFor: widget.
	self shortcut ifNotNil: [ :s | self registerShortcut: s ]