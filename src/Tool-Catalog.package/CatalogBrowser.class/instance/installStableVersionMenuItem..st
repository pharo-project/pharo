menu
installStableVersionMenuItem: aList
	<contextMenu>
	aList 
		selectionAct: [ :presenter | self onInstallFrom: presenter ] 
		icon: (Smalltalk ui  icons iconNamed: #glamorousAccept)
		entitled: 'Install stable version'