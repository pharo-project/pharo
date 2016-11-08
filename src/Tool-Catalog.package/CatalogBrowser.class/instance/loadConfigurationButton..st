menu
loadConfigurationButton: aList
	<menuOrder: 4>
	aList
		act: [ :presenter | self onLoadFrom: presenter ]
		iconName: #configIcon
		entitled: 'Load configuration (do not install project)'