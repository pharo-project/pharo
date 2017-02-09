*Glamour-FastTable
fastListDoubleHeight
	"self new fastListDoubleHeight openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List With Double Height' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList 
			display: Smalltalk allClassesAndTraits;
			rowHeight: [ :presentation | presentation class defaultRowHeight * 2 ] ].
	^ browser