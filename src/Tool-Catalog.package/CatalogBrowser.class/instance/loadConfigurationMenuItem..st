menu
loadConfigurationMenuItem: aList
	<contextMenu>
	aList 
		selectionAct: [ :presenter | self onLoadFrom: presenter ] 
		icon: (self iconNamed: #configIcon)
		entitled: 'Load configuration (do not install project)'