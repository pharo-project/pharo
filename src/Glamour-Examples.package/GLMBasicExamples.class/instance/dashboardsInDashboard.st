browsers
dashboardsInDashboard

	<glmBrowser: 'Dashboards in dashboard' input: '42'>
	"self new dashboard openOn: 42"
	
	|tmpBrowser|
	tmpBrowser := GLMDashboard new.
	tmpBrowser addPaneNamed: #dashboard.
	tmpBrowser addPaneNamed: #second.
	tmpBrowser addPaneNamed: #third.
	
	tmpBrowser transmit to: #dashboard; andShow: [:a |
		a custom: (self dashboard)
		].
	tmpBrowser transmit to: #second; andShow: [:a |
		a custom: (self dashboard)
		].
	tmpBrowser transmit to: #third; andShow: [:a |
		a custom: (self dashboard)
		].
	^ tmpBrowser