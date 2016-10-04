private
validate
	form ifNil: [ ^ false ].
	(renderer isNil or: [renderer isNull]) ifTrue: [ self createRenderer ].
	(texture isNil or: [texture isNull or: [ form extent ~= extent ]])
		ifTrue: [ self createTexture ].
	^ true