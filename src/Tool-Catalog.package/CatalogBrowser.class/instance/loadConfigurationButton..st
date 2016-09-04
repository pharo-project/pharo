menu
loadConfigurationButton: aList
	<menu>
	aList
		act: [ :presenter | self onLoadFrom: presenter ]
		icon: (self iconNamed: #configIcon)
		entitled: 'Load configuration (do not install project)'