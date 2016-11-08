menu
installStableVersionMenuItem: aList
	<contextMenu>
	aList 
		selectionAct: [ :presenter | self onInstallFrom: presenter ] 
		iconName: #glamorousAccept
		entitled: 'Install stable version'