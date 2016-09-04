Nautilus groups support
installAndCreateGroupButton: aList
	<menu>
	aList
		act: [ :presenter | self onInstallAndCreateGroupFrom: presenter ]
		icon: (self iconNamed: #publishIcon)
		entitled: 'Install stable version and create group'