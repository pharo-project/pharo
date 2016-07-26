instance creation
default
	^ Singleton ifNil: [ Singleton := self new ]