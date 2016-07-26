explicit selection
listsInDashboardWithUpdatedSelection
	"self new listsInDashboardWithUpdatedSelection openOn: 10"
	<glmBrowser: 'Updated selection in dashboard' input: '10'>
	| browser |
	browser := GLMDashboard new.
	browser addPaneNamed: #one; addPaneNamed: #two; addPaneNamed: #three.
	browser transmit to: #one; andShow: [ :a | 
		a tree
			display: [:x | 1 to: x ]].
	browser transmit to: #two; andShow: [ :a | 
		a tree
			display: [:x | 1 to: x + 2 ]].
	browser transmit to: #three; andShow: [ :a |
		a tree
			beMultiple;
			display: [:x | 1 to: x ]].
	browser transmit to: #two port: #selection; from: #one; transformed: [:x | x + 2].
	browser transmit to: #one port: #selection; from: #two; transformed: [:x | x - 2].
	browser transmit to: #three port: #selection; from: #two; transformed: [:x | (1 to: (x - 2)) ].
	^ browser