Nautilus groups support
installAndCreateGroupMenuItem: aList
	<contextMenu>
	aList selectionAct: [ :presenter | self onInstallAndCreateGroupFrom: presenter ] entitled: 'Install stable version and create group' 