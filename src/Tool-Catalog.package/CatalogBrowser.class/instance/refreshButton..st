building
refreshButton: aList
	<menuOrder: 10>
	aList
		act: [ :presenter | self onRefreshFrom: presenter ]
		iconName: #glamorousRefresh
		entitled: 'Refresh'