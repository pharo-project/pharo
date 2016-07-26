as yet unclassified
computeWidth

	^ self pagerModel
		ifNil: [ 0 ]
		ifNotNil: [ :m | m circleButtonSize * m actualNumberOfVisiblePages ]