updating screen
validate
	self checkSession.
	(texture isNil or: [ texture isNull ]) ifTrue: [ ^ false ].	
	window extent ~= textureExtent ifTrue: [ ^ false ].
	^ true