api
setExtentAndBindingTo: widget

	self ensureExtentFor: widget.
	self ensureKeyBindingsFor: widget.
	self announce: (WidgetBuilt model: self widget: widget).
	