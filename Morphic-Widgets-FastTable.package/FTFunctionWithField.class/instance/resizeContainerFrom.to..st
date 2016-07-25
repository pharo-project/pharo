private
resizeContainerFrom: topLeftPoint to: bottomLeftPoint
	^ topLeftPoint corner: bottomLeftPoint x @ (bottomLeftPoint y - self fieldHeigh)