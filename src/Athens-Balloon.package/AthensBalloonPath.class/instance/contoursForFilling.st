converting
contoursForFilling
	"answer a collection of contours, ready for filling by balloon engine"
	^ contours collect: [:contour |
		contour last = contour first 
		ifTrue: [contour asArray] 
		ifFalse: [ (contour , { contour last. contour last. contour first }) ]
	]