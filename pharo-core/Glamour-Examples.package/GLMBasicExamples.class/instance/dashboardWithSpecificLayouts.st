browsers
dashboardWithSpecificLayouts
	"self new dashboardWithSpecificLayouts openOn: 42"

	<glmBrowser: 'Dashboard specifying layouts' input: '42'>
	| tmpBrowser |
	tmpBrowser := GLMDashboard new.
	tmpBrowser addPaneNamed: #first layout: (0 @ 0 corner: 0.33 @ 0.5).
	tmpBrowser addPaneNamed: #second layout: (0.33 @ 0 corner: 0.66 @ 0.5).
	tmpBrowser addPaneNamed: #third layout: (0 @ 0.5 corner: 0.33 @ 1).
	tmpBrowser addPaneNamed: #fourth layout: (0.33 @ 0.5 corner: 0.66 @ 1).
	tmpBrowser addPaneNamed: #fifth layout: (0.66 @ 0 corner: 1 @ 1).
	tmpBrowser transmit
		to: #first;
		andShow: [ :a | 
					a list
						display: [ :i | #(#a #b #c #d #e) ];
						yourself ].
	tmpBrowser transmit
		to: #second;
		andShow: [ :a | a list display: [ :i | #(#a #b #c #d #e) ] ].
	tmpBrowser transmit
		to: #third;
		andShow: [ :a | a list display: [ :i | #(#a #b #c #d #e) ] ].
	tmpBrowser transmit
		to: #fourth;
		andShow: [ :a | a list display: [ :i | #(#a #b #c #d #e) ] ].
	tmpBrowser transmit
		to: #fifth;
		andShow: [ :a | a list display: [ :i | #(#a #b #c #d #e) ] ].
	^ tmpBrowser