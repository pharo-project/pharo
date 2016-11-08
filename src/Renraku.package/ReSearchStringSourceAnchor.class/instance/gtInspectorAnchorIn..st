gt-inspector-extension
gtInspectorAnchorIn: composite
	<gtInspectorPresentationOrder: 0>

	(super gtInspectorAnchorIn: composite)
		initialize: [ :p | p selectionInterval: self interval ]