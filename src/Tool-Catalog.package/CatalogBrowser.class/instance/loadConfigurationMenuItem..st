menu
loadConfigurationMenuItem: aList
	<contextMenu>
	aList 
		selectionAct: [ :presenter | self onLoadFrom: presenter ] 
		iconName: #configIcon
		entitled: 'Load configuration (do not install project)'