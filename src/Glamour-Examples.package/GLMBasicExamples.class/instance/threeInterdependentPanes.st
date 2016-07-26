others
threeInterdependentPanes
	"self new threeInterdependentPanes openOn: 5"

	<glmBrowser: 'Three inter-dependent panes' input: '5'>
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		to: #one;
		andShow: [ :a | a tree display: [ :x | 1 to: x ] ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | a tree display: [ :x | 1 to: x * 2 ] ].
	(browser transmit)
		to: #three;
		from: #two;
		andShow: [ :a | 
					(a tree)
						display: [ :x | 1 to: x * 3 ];
						populate: #focus
							on: $f
							entitled: 'Focus'
							with: [ :list | list selection ] ].
	(browser transmit)
		toOutsidePort: #focus;
		from: #three port: #focus.
	(browser transmit)
		to: #one port: #selection;
		fromOutsidePort: #focus.
	(browser transmit)
		to: #two port: #selection;
		fromOutsidePort: #focus.
	^ browser