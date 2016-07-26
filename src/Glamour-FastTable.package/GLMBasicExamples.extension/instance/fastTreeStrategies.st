*Glamour-FastTable
fastTreeStrategies
	"self new fastTreeStrategies openOn: (GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])"

	<glmBrowser: 'Fast Tree Function Strategies' input: '(GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a fastTree
				title: 'Fast Tree Filter Visibles';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableFilter; "Could be search also"
				strategy: #default. "Optional in this case."
			a fastTree
				title: 'Fast Tree Filter Roots';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableFilter;
				strategy: #rootsOnly.
			a fastTree
				title: 'Fast Tree Filter All';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableFilter;
				strategy: #allItems.
			a fastTree
				title: 'Fast Tree Search All';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				enableSearch;
				strategy: #allItems.
				 ].
	^ browser