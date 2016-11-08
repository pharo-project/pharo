menu
installStableVersionButton: aList
	<menuOrder: 2>
	aList
		act: [ :presenter | self onInstallFrom: presenter ]
		iconName: #glamorousAccept
		entitled: 'Install stable version'