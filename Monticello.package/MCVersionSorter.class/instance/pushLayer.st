private
pushLayer
	depthIndex := depthIndex + 1.
	depthIndex > layers size ifTrue: [layers add: OrderedCollection new].
	