menu
loadConfigurationMenuItem: aList
	<contextMenu>
	aList 
		selectionAct: [ :presenter | self onLoadFrom: presenter ] 
		icon: (Smalltalk ui icons iconNamed: #configIcon)
		entitled: 'Load configuration (do not install project)'