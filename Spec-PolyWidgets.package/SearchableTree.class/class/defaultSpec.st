specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [:r |
			r
			"add: #label width: 50;"
			add: #inputField;
			add: #button width: 25 ] height: 25;
		newRow: #tree top: 25;
		yourself