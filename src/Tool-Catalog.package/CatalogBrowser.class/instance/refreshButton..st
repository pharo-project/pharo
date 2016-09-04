building
refreshButton: aList
	<menu>
	aList
		act: [ :presenter | self onRefreshFrom: presenter ]
		icon: (self iconNamed: #glamorousRefresh)
		entitled: 'Refresh'