*Iceberg-UI
icon
	^ Smalltalk ui icons iconNamed: 
		(self hasChanges 
			ifTrue: [ #dirtyMonticelloPackage ]
			ifFalse: [ #emptyPackage  ])
