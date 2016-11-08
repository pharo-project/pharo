widget protocol
switchAutoscale
	| layoutValue |
	layoutValue := self model autoScale
		ifTrue: [ #scaledAspect ]
		ifFalse: [ #center ].
	self widgetDo: [ :w | w layout: layoutValue ]