Nautilus groups support
installAndCreateGroupButton: aList
	<menu>
	aList
		act: [ :presenter | self onInstallAndCreateGroupFrom: presenter ]
		icon: (Smalltalk ui icons iconNamed: #publishIcon)
		entitled: 'Install stable version and create group'