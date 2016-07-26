*Glamour-FastTable
fastTreeSimple
	"self new fastTreeSimple openOn: (GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])"

	<glmBrowser: 'Fast Tree Simple' input: '(GLMPresentation allSubclasses sort: [:a :b | a asString < b asString])'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a fastTree
				title: 'Fast Tree';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ].
			a fastTree
				title: 'Fast Expanded';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				allExpanded.
			a fastTree
				title: 'Fast Tree Root Expanded';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				rootsExpanded.
			a fastTree
				title: 'Fast Tree Expanded Until 1';
				children: [ :class | class subclasses sort: [ :b :c | b asString < c asString ] ];
				expandedUntil: 1.
				 ].
	^ browser