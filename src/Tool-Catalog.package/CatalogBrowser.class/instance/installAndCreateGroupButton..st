Nautilus groups support
installAndCreateGroupButton: aList
	<menuOrder: 1>
	aList
		act: [ :presenter | self onInstallAndCreateGroupFrom: presenter ]
		iconName: #publishIcon
		entitled: 'Install stable version and create group'