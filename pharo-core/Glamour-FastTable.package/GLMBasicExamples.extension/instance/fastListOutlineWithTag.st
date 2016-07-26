*Glamour-FastTable
fastListOutlineWithTag
	"self new fastListOutlineWithTag openOn: Object"

	<glmBrowser: 'Fast List Outline with Tag' input: 'Object'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow:
				[ :a | 
			a fastList
				title: 'Example with an Outline List';
				display: [ :x | x allSubclasses collect: [ :each | each allSubclasses ] ];
				withSeparators;
				tags: [ :item | item asString first asString ] ].
	^ browser