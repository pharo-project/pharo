instance creation
singleton 
	^ Singleton ifNil: [Singleton := self new].
