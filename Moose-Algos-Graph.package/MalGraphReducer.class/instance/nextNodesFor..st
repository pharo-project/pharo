computing
nextNodesFor: aNodeList
	"compute next nodes for the future node replacing aNodeList"
	^ ((aNodeList flatCollect: #nextNodes as: Set)
		difference: aNodeList) asOrderedCollection