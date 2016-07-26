menu
installStableVersionButton: aList
	<menu>
	aList
		act: [ :presenter | self onInstallFrom: presenter ]
		icon: (Smalltalk ui icons iconNamed: #glamorousAccept)
		entitled: 'Install stable version'