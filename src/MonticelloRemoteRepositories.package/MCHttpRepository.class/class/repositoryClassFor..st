instance creation
repositoryClassFor: location
	MCHttpRepository subclassesDo: [ :subclass | 
		(subclass isResponsibleFor: location)
			ifTrue: [ ^ subclass ]].
	^ MCHttpRepository