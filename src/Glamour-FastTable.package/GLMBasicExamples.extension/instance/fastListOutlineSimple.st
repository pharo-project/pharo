*Glamour-FastTable
fastListOutlineSimple
	"self new fastListOutlineSimple openOn: Object"

	<glmBrowser: 'Fast List Outline Simple' input: 'Object'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | 
			a fastList
				title: 'Example with an Outline List';
				display: [ :x | x allSubclasses collect: [ :each | each allSubclasses ] ];
				withSeparators ].
	^ browser