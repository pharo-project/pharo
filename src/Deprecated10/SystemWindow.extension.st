Extension { #name : #SystemWindow }

{ #category : #'*Deprecated10' }
SystemWindow >> openInWorldExtent: extent [

	"This msg and its callees result in the window being activeOnlyOnTop"

	self
		deprecated: 'Use #extent: instead'
		transformWith:
		'`@rec openInWorldExtent: `@arg' -> '`@rec extent: `@arg'.
	^ self extent: extent
]
