menus
setRotationCenterFrom: aPoint
	self rotationCenter: (aPoint - self bounds origin) / self bounds extent asFloatPoint.