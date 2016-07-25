managing pool users
removeUser: aRGClassDefinition
	"Removes this RGClassDefinition from the users of the receiver"
	
	aRGClassDefinition isClass ifFalse:[ ^self ].
	aRGClassDefinition theNonMetaClass removeSharedPoolNamed: self name.
	self users remove: aRGClassDefinition theNonMetaClass ifAbsent:[ ]