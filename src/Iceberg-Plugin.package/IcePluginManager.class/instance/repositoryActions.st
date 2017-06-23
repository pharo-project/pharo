accessing ui
repositoryActions
	^ self plugins 
		inject: #() 
		into: [ :all :each | all, (each repositoryActions) ]	
		