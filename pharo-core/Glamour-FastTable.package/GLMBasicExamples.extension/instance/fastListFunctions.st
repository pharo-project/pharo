*Glamour-FastTable
fastListFunctions
	"self new fastListFunctions openOn: ((Smalltalk allClassesAndTraits sort: [ :a :b | a asString < b asString ]) asArray)"

	"This can be done for a fastlist, a fastlist with separators, a fasttable and even a fastftree."

	<glmBrowser: 'Fast List with function' input: '(Smalltalk allClassesAndTraits sort: [ :a :b | a asString < b asString ]) asArray '>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow:
				[ :a | 
			a fastList
				title: 'Search';
				display: [ :input | input ];
				enableSearch.
			a fastList
				title: 'Filter';
				display: [ :input | input ];
				enableFilter.
			a fastList
				title: 'Filter and action';
				display: [ :input | input ];
				enableFilterWithAction: [ :dataSource :pattern :selectedRow | selectedRow browse ].
			a fastList
				title: 'Filter and named action ';
				display: [ :input | input ];
				enableFilterWithAction: [ :dataSource :pattern :selectedRow | selectedRow browse ] named: 'Browse' ].
	^ browser