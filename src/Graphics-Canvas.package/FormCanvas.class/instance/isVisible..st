testing
isVisible: aRectangle
	"Optimization"
	(aRectangle right + origin x) < clipRect left	ifTrue: [^ false].
	(aRectangle left + origin x) > clipRect right	ifTrue: [^ false].
	(aRectangle bottom + origin y) < clipRect top	ifTrue: [^ false].
	(aRectangle top + origin y) > clipRect bottom	ifTrue: [^ false].
	^ true
