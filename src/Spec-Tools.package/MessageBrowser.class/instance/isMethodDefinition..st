private
isMethodDefinition: anItem
	"an item can be nil, a compiled method, or a ring object. Whereas a ring object can be a ring method or comment definition.
	 Some operations are only valid for compiled methods or ring object that are a method definitions"

	anItem ifNil: [ ^ false ].
	
	^ (anItem isRingObject and: [ anItem isMethod not 
		or: [ anItem compiledMethod isNotNil ] ]
	)
	"assume it is a compiled method or ring method definition"

