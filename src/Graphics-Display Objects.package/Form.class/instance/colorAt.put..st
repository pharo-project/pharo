pixel access
colorAt: aPoint put: aColor
	"Store a Color into the pixel at coordinate aPoint.  "

	self pixelValueAt: aPoint put: (self pixelValueFor: aColor).

"[Sensor anyButtonPressed] whileFalse:
	[Display colorAt: Sensor cursorPoint put: Color red]"
