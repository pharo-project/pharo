private
headerStep

	decoder decodeYourself.
	clusterCount := decoder nextEncodedPositiveInteger.
	clusters := OrderedCollection new: clusterCount.
	clusters resetTo: 1. "Hack that avoids OrderedCollection>>makeRoomAtLast"