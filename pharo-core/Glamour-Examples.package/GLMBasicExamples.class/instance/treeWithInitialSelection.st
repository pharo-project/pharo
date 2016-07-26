explicit selection
treeWithInitialSelection
	"
		| browser |
		browser := self new treeWithInitialSelection. 
		browser openOn: {
			#first->{$a->{}. $c->{}. $d->{}}.
			#second->{$e->{}. $f->{}}. 
			#third->{$h->{}}
		}.
		(browser panes first port: #selection) value: (browser panes first port: #entity) value first value first.
	"

	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: [ :c | 
					c
						row: #two;
						row: #three ].
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a tree)
						title: 'first tree';
						children: [ :x | x value ].
					(a tree)
						title: 'second tree';
						children: [ :x | x value ] ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | a text title: 'Selection preview' ].
	(browser transmit)
		to: #three;
		from: #one port: #selectionPath;
		andShow: [ :a | a text title: 'Selection path preview' ].
	^ browser