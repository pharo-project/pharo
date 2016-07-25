private
validate
	form ifNil: [ ^ false ].
	renderer ifNil: [ self createRenderer ].
	(texture isNil or: [ form extent ~= extent ])
		ifTrue: [ self createTexture ].
	^ true