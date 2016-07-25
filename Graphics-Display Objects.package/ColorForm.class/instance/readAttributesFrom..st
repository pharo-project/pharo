filein/out
readAttributesFrom: aBinaryStream
	super readAttributesFrom: aBinaryStream.
	colors := ColorArray new: (2 raisedTo: depth).
	1 to: colors size do: [:idx | 
		colors basicAt: idx put: (aBinaryStream nextLittleEndianNumber: 4).
	]. 
	