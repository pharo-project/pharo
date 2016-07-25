menu
analyzeSubMenu: aBuilder
	<contextMenu>
	<nautilusGlobalClassMenu>
	| target |
	
	target := aBuilder model.
	target selectedClass ifNil: [^target].
	target selectedClass buildAnalyzeSubMenu: aBuilder.