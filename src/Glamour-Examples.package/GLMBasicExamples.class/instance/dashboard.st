browsers
dashboard

	<glmBrowser: 'Dashboard' input: '42'>
	"self new dashboard openOn: 42"
	
	|tmpBrowser|
	tmpBrowser := GLMDashboard new.
	tmpBrowser addPaneNamed: #first.
	tmpBrowser addPaneNamed: #second.
	tmpBrowser addPaneNamed: #third.
	tmpBrowser addPaneNamed: #fourth.
	tmpBrowser addPaneNamed: #fifth.
	
	tmpBrowser transmit to: #first; andShow: [:a |
		a list
			display: [:i | #(a b c d e f g)];
			yourself
		].
	tmpBrowser transmit to: #second; andShow: [:a |
		a list
			display: [:i | #(a b c d e f g)]
		].
	tmpBrowser transmit to: #third; andShow: [:a |
		a list
			display: [:i | #(a b c d e f g)]
		].
	tmpBrowser transmit to: #fourth; andShow: [:a |
		a list
			display: [:i | #(a b c d e f g)]
		].
	tmpBrowser transmit to: #fifth; andShow: [:a |
		a list
			display: [:i | #(a b c d e f g h i j k l m n o p q r s t u v w x y z)]
		].
	^ tmpBrowser