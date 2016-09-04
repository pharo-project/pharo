menu
installStableVersionButton: aList
	<menu>
	aList
		act: [ :presenter | self onInstallFrom: presenter ]
		icon: (self iconNamed: #glamorousAccept)
		entitled: 'Install stable version'