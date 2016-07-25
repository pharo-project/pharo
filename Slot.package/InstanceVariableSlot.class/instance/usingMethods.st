queries
usingMethods
	"All methods that read or write the slot, we can do it here without having to use the AST"
	^self definingClass 
		ifNil: [ #() ]
		ifNotNil: [ :definingClass | definingClass withAllSubclasses flatCollect: [:class |  
		(class whichSelectorsAccess: self name) collect: [:sel | 
			class compiledMethodAt: sel ]]]