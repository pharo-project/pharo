accessing
repositoriesLocation
 	^ self shareRepositoriesBetweenImages 
		ifTrue: [ self sharedRepositoriesLocation ]
		ifFalse: [ self localRepositoriesLocation ] 