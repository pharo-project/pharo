geometry
align: aPoint1 with: aPoint2
	"Translate by aPoint2 - aPoint1."

	^ self position: self position + (aPoint2 - aPoint1)