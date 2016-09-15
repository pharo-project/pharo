private
canvasPointFor: aNode
	^ (rulerBuilder centerOfColumnIndex: aNode rulerPoint x) @ (rulerBuilder centerOfRowIndex: aNode rulerPoint y)