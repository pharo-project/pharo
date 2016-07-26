operations
descriptionWith: anECContext 
	description ifNotNil: [ ^ description ].
	^ description := self createDescriptionWith: anECContext