menu
codeRewritingClassSubmenu: aBuilder
	<nautilusGlobalClassMenu>
	| target |
	target := aBuilder model.
	target selectedClass ifNil: [^target].
	
	target selectedClass codeRewritingClass: aBuilder.