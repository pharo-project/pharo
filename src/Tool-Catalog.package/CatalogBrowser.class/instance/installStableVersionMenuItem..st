menu
installStableVersionMenuItem: aList
	<contextMenu>
	aList 
		selectionAct: [ :presenter | self onInstallFrom: presenter ] 
		icon: (self iconNamed: #glamorousAccept)
		entitled: 'Install stable version'