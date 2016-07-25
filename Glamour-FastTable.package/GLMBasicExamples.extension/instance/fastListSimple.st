*Glamour-FastTable
fastListSimple
	"self new fastListSimple openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List Simple' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList display: Smalltalk allClassesAndTraits ].
	^ browser