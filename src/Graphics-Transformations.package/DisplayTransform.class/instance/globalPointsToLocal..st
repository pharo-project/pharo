transforming points
globalPointsToLocal: inArray
	"Transform all the points of inArray from global into local coordinates"
	^inArray collect:[:pt| self globalPointToLocal: pt]