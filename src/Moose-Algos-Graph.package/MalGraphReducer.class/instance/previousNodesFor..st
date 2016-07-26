computing
previousNodesFor: aNodeList
	"compute previous nodes for the future node replacing aNodeList"
	^ ((aNodeList flatCollect: #previousNodes as: Set)
		difference: aNodeList) asOrderedCollection