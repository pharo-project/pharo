building
refreshButton: aList
	<menu>
	aList
		act: [ :presenter | self onRefreshFrom: presenter ]
		icon: (Smalltalk ui icons iconNamed: #glamorousRefresh)
		entitled: 'Refresh'