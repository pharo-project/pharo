private
canvasPointsForAll: aCollectionOfNodes
	^ aCollectionOfNodes collect: [ :rulerPoint | 
		(rulerBuilder centerOfColumnIndex: rulerPoint x) @ (rulerBuilder centerOfRowIndex: rulerPoint y) ]