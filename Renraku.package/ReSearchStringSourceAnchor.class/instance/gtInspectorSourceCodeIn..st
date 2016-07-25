gt-inspector-extension
gtInspectorSourceCodeIn: composite
	<gtInspectorPresentationOrder: 0>

	super gtInspectorSourceCodeIn: composite.
	
	composite presentations
		select: [ :p | p isKindOf: GLMRubricSmalltalkCodePresentation ] 
		thenDo: [ :p | p initialize: [ :p2 |
			p2 selectionInterval: self interval ] ]